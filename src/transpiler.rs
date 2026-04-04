use regex::Regex;
use std::collections::HashMap;
use tree_sitter::Node;
use oxc_allocator::Allocator;
use oxc_ast::ast::Program;
use oxc_codegen::Codegen;
use oxc_parser::Parser;
use oxc_span::SourceType;

/// Main transpiler struct that converts Kotlin to JavaScript
pub struct KotlinTranspiler;

impl KotlinTranspiler {
    /// Transpile Kotlin code to JavaScript
    pub fn transpile(kotlin_code: &str, tree: &tree_sitter::Tree) -> String {
        let root = tree.root_node();
        let raw_js = Self::sanitize_output_js(&Self::transpile_node(&root, kotlin_code));
        let allocator = Allocator::default();
        if let Some(program) = Self::transpile_to_oxc_program(&allocator, &raw_js) {
            Codegen::new().build(&program).code
        } else {
            raw_js.trim_end().to_string() + "\n"
        }
    }

    /// Convert generated JS snippet into an Oxc JavaScript Program AST.
    pub fn transpile_to_oxc_program<'a>(
        allocator: &'a Allocator,
        raw_js: &'a str,
    ) -> Option<Program<'a>> {
        let source_type = SourceType::mjs();
        let parsed = Parser::new(allocator, raw_js, source_type).parse();

        if parsed.panicked || !parsed.errors.is_empty() {
            return None;
        }

        Some(parsed.program)
    }

    /// Recursively transpile a tree-sitter node
    fn transpile_node(node: &Node, source: &str) -> String {
        match node.kind() {
            "source_file" => Self::transpile_source_file(node, source),
            "package_header" => String::new(), // Packages don't exist in JS
            "import" => Self::transpile_import(node, source),
            "import_header" => Self::transpile_import(node, source),
            "function_declaration" => Self::transpile_function(node, source),
            "class_declaration" => Self::transpile_class(node, source),
            "property_declaration" => Self::transpile_property(node, source),
            "variable_declaration" => Self::transpile_variable(node, source),
            "call_expression" => Self::transpile_call(node, source),
            "value_argument" => Self::transpile_value_argument(node, source),
            "string_literal" => Self::transpile_string_literal(node, source),
            "number_literal" => Self::get_node_text(node, source),
            "line_comment" => String::new(),
            "block_comment" => String::new(),
            "simple_identifier" => Self::get_node_text(node, source),
            "identifier" => Self::get_node_text(node, source),
            "navigation_expression" => {
                Self::transpile_apply_let_chain(node, source)
                    .unwrap_or_else(|| Self::sanitize_js_text(&Self::get_node_text(node, source)))
            }
            "lambda_literal" => Self::transpile_lambda(node, source),
            "if_expression" => Self::transpile_if(node, source),
            "when_expression" => Self::transpile_when(node, source),
            "for_statement" => Self::transpile_for(node, source),
            "while_statement" => Self::transpile_while(node, source),
            "do_while_statement" => Self::transpile_do_while(node, source),
            "assignment" => Self::transpile_assignment(node, source),
            "binary_expression" => Self::transpile_binary(node, source),
            "postfix_expression" => Self::transpile_postfix(node, source),
            "prefix_expression" => Self::transpile_prefix(node, source),
            "companion_object" => String::new(),
            "object_declaration" => Self::transpile_object(node, source),
            "block" => Self::transpile_block(node, source),
            _ => Self::sanitize_js_text(&Self::get_node_text(node, source)),
        }
    }

    fn transpile_value_argument(node: &Node, source: &str) -> String {
        let mut seen_equals = false;
        let mut first_non_empty: Option<String> = None;
        let child_count = node.child_count();
        for i in 0..child_count {
            if let Some(child) = node.child(i as u32) {
                if child.is_extra() || child.kind() == "," {
                    continue;
                }

                if child.kind() == "=" {
                    seen_equals = true;
                    continue;
                }

                let out = Self::transpile_node(&child, source);
                if !out.trim().is_empty() {
                    if seen_equals {
                        return out;
                    }
                    if first_non_empty.is_none() {
                        first_non_empty = Some(out);
                    }
                }
            }
        }
        first_non_empty.unwrap_or_default()
    }

    fn transpile_source_file(node: &Node, source: &str) -> String {
        let mut result = String::new();
        let child_count = node.child_count();
        for i in 0..child_count {
            if let Some(child) = node.child(i as u32) {
                if !child.is_extra() && child.kind() != "ERROR" {
                    let child_output = Self::transpile_node(&child, source);
                    if !child_output.trim().is_empty() {
                        result.push_str(&child_output);
                        result.push('\n');
                    }
                }
            }
        }
        result
    }

    fn transpile_import(node: &Node, source: &str) -> String {
        let raw = Self::get_node_text(node, source).trim().to_string();
        let path = raw.strip_prefix("import").unwrap_or(&raw).trim();

        if path.is_empty() {
            return String::new();
        }

        let (path, alias) = match path.split_once(" as ") {
            Some((import_path, import_alias)) => (import_path.trim(), Some(import_alias.trim())),
            None => (path, None),
        };

        if let Some(module_path) = path.strip_suffix(".*") {
            return format!(
                "import * as {} from \"{}\";\n",
                Self::sanitize_js_identifier(module_path),
                module_path
            );
        }

        let mut parts: Vec<&str> = path.split('.').collect();
        if parts.len() < 2 {
            return format!("import \"{}\";\n", path);
        }

        let symbol_name = parts.pop().unwrap_or_default();
        let module_path = parts.join(".");

        match alias {
            Some(import_alias) => format!(
                "import {{ {} as {} }} from \"{}\";\n",
                symbol_name,
                import_alias,
                module_path
            ),
            None => format!(
                "import {{ {} }} from \"{}\";\n",
                symbol_name,
                module_path
            ),
        }
    }

    fn sanitize_js_identifier(value: &str) -> String {
        let mut identifier = value
            .chars()
            .map(|ch| if ch.is_ascii_alphanumeric() || ch == '_' { ch } else { '_' })
            .collect::<String>();

        if identifier
            .chars()
            .next()
            .map(|ch| ch.is_ascii_digit())
            .unwrap_or(true)
        {
            identifier.insert(0, '_');
        }

        identifier
    }

    fn transpile_function(node: &Node, source: &str) -> String {
        let mut result = String::from("async function ");
        let mut found_name = false;
        let mut found_params = false;
        let mut found_returns = false;
        let child_count = node.child_count();

        for i in 0..child_count {
            if let Some(child) = node.child(i as u32) {
                match child.kind() {
                    "identifier" if !found_name => {
                        result.push_str(&Self::get_node_text(&child, source));
                        found_name = true;
                    }
                    "function_value_parameters" if !found_params => {
                        result.push_str(&Self::transpile_parameters(&child, source));
                        found_params = true;
                    }
                    "type_reference" if !found_returns => {
                        // Skip return type annotation, not needed in JS
                        found_returns = true;
                    }
                    "function_body" => {
                        result.push_str(&Self::transpile_function_body(&child, source));
                    }
                    _ => {}
                }
            }
        }

        format!("{}\n", result)
    }

    fn transpile_function_body(node: &Node, source: &str) -> String {
        let child_count = node.child_count();

        // function_body contains a block node
        for i in 0..child_count {
            if let Some(child) = node.child(i as u32) {
                if child.kind() == "block" {
                    return Self::transpile_block(&child, source);
                }
            }
        }

        let mut body_expr = String::new();

        for i in 0..child_count {
            if let Some(child) = node.child(i as u32) {
                match child.kind() {
                    "=" => {}
                    _ if !child.is_extra() && child.kind() != "type_reference" => {
                        let text = Self::transpile_node(&child, source);
                        if !text.trim().is_empty() {
                            if !body_expr.is_empty() {
                                body_expr.push(' ');
                            }
                            body_expr.push_str(text.trim());
                        }
                    }
                    _ => {}
                }
            }
        }

        if !body_expr.trim().is_empty() {
            return format!(" {{\n  return {};\n}}\n", body_expr.trim());
        }

        // Fallback if no block or expression body found
        String::from(" {\n}\n")
    }

    fn transpile_parameters(node: &Node, source: &str) -> String {
        let mut result = String::from("(");
        let mut first = true;
        let child_count = node.child_count();

        for i in 0..child_count {
            if let Some(child) = node.child(i as u32) {
                if child.kind() == "function_value_parameter" || child.kind() == "parameter" {
                    if !first {
                        result.push_str(", ");
                    }
                    if let Some(parameter_name) = Self::find_first_identifier_text(&child, source) {
                        result.push_str(&parameter_name);
                    }
                    first = false;
                }
            }
        }

        result.push(')');
        result
    }

    fn transpile_class(node: &Node, source: &str) -> String {
        let mut result = String::from("class ");
        let mut super_class: Option<String> = None;

        let class_text = Self::get_node_text(node, source);
        let class_header = class_text.split('{').next().unwrap_or(&class_text).trim();

        if let Some(class_name) = Regex::new(r"\bclass\s+([A-Za-z_][A-Za-z0-9_]*)")
            .expect("valid class name regex")
            .captures(class_header)
            .and_then(|caps| caps.get(1))
            .map(|m| m.as_str().to_string())
        {
            result.push_str(&class_name);
        }

        if let Some(colon_idx) = class_header.find(':') {
            let header_tail = &class_header[colon_idx + 1..];
            let header_tail = header_tail.split('{').next().unwrap_or(header_tail).trim();
            let first_super = header_tail.split(',').next().unwrap_or("").trim();
            if !first_super.is_empty() {
                let super_name = first_super
                    .split('(')
                    .next()
                    .unwrap_or(first_super)
                    .trim();
                let super_name = super_name
                    .split('<')
                    .next()
                    .unwrap_or(super_name)
                    .trim();
                if !super_name.is_empty() {
                    super_class = Some(super_name.to_string());
                    result.push_str(" extends ");
                    result.push_str(super_name);
                }
            }
        }

        for i in 0..node.child_count() {
            if let Some(child) = node.child(i as u32) {
                match child.kind() {
                    "class_body" => {
                        result.push_str(&Self::transpile_class_body(
                            &child,
                            source,
                            super_class.as_deref(),
                        ));
                    }
                    _ => {}
                }
            }
        }

        format!("{}\n", result)
    }

    fn transpile_object(node: &Node, source: &str) -> String {
        let mut result = String::from("class ");
        let mut object_body: Option<Node> = None;

        if let Some(object_name) = Regex::new(r"\bobject\s+([A-Za-z_][A-Za-z0-9_]*)")
            .expect("valid object name regex")
            .captures(&Self::get_node_text(node, source))
            .and_then(|caps| caps.get(1))
            .map(|m| m.as_str().to_string())
        {
            result.push_str(&object_name);
        }

        for i in 0..node.child_count() {
            if let Some(child) = node.child(i as u32) {
                if child.kind() == "class_body" {
                    object_body = Some(child);
                    break;
                }
            }
        }

        if let Some(body) = object_body {
            result.push_str(&Self::transpile_object_body(&body, source));
        } else {
            result.push_str(" {\n}\n");
        }

        format!("{}\n", result)
    }

    fn transpile_property(node: &Node, source: &str) -> String {
        Self::transpile_binding_declaration(node, source)
    }

    fn transpile_class_body(node: &Node, source: &str, super_class: Option<&str>) -> String {
        let mut result = String::from(" {\n");
        let mut overload_groups: HashMap<String, Vec<(Vec<String>, String)>> = HashMap::new();

        for i in 0..node.child_count() {
            if let Some(child) = node.child(i as u32) {
                if child.kind() == "{" || child.kind() == "}" || child.is_extra() {
                    continue;
                }

                match child.kind() {
                    "function_declaration" => {
                        if let Some((name, params, body)) = Self::parse_class_method_parts(&child, source)
                        {
                            overload_groups
                                .entry(name)
                                .or_default()
                                .push((params, body));
                        }
                    }
                    "property_declaration" | "variable_declaration" => {
                        let field = Self::transpile_binding_declaration(&child, source);
                        let field = field.trim_start_matches("let ");
                        result.push_str("  ");
                        result.push_str(field);
                    }
                    "companion_object" | "class_declaration" | "object_declaration" => {}
                    _ => {
                        let out = Self::transpile_node(&child, source);
                        if !out.trim().is_empty() {
                            result.push_str("  ");
                            result.push_str(&out);
                        }
                    }
                }
            }
        }

        if super_class.is_some() {
            result.push_str("  constructor(...args) {\n");
            result.push_str("    super(...args);\n");
            result.push_str("  }\n");
        }

        for (name, overloads) in overload_groups {
            result.push_str("  ");
            result.push_str(&Self::emit_class_method_with_overloads(&name, &overloads));
        }

        result.push_str("}\n");
        result
    }

    fn transpile_object_body(node: &Node, source: &str) -> String {
        let mut result = String::from(" {\n");
        let mut overload_groups: HashMap<String, Vec<(Vec<String>, String)>> = HashMap::new();

        for i in 0..node.child_count() {
            if let Some(child) = node.child(i as u32) {
                if child.kind() == "{" || child.kind() == "}" || child.is_extra() {
                    continue;
                }

                match child.kind() {
                    "function_declaration" => {
                        if let Some((name, params, body)) = Self::parse_class_method_parts(&child, source) {
                            overload_groups
                                .entry(name)
                                .or_default()
                                .push((params, body));
                        }
                    }
                    "property_declaration" | "variable_declaration" => {
                        let field = Self::transpile_binding_declaration(&child, source);
                        let field = field.trim_start_matches("let ");
                        result.push_str("  static ");
                        result.push_str(field);
                    }
                    "class_declaration" | "object_declaration" | "companion_object" => {}
                    _ => {
                        let out = Self::transpile_node(&child, source);
                        if !out.trim().is_empty() {
                            result.push_str("  ");
                            result.push_str(&out);
                        }
                    }
                }
            }
        }

        for (name, overloads) in overload_groups {
            result.push_str("  static ");
            result.push_str(&Self::emit_class_method_with_overloads(&name, &overloads));
        }

        result.push_str("}\n");
        result
    }

    fn parse_class_method_parts(node: &Node, source: &str) -> Option<(String, Vec<String>, String)> {
        let mut method_name: Option<String> = None;
        let mut params: Vec<String> = Vec::new();
        let mut body = String::from(" {\n}\n");

        for i in 0..node.child_count() {
            if let Some(child) = node.child(i as u32) {
                match child.kind() {
                    "identifier" if method_name.is_none() => {
                        method_name = Some(Self::get_node_text(&child, source));
                    }
                    "function_value_parameters" => {
                        params = Self::extract_parameter_names(&child, source);
                    }
                    "function_body" => {
                        body = Self::transpile_function_body(&child, source);
                    }
                    _ => {}
                }
            }
        }

        method_name.map(|name| (name, params, body))
    }

    fn extract_parameter_names(node: &Node, source: &str) -> Vec<String> {
        let mut params = Vec::new();

        for i in 0..node.child_count() {
            if let Some(child) = node.child(i as u32) {
                if child.kind() == "function_value_parameter" || child.kind() == "parameter" {
                    if let Some(name) = Self::find_first_identifier_text(&child, source) {
                        params.push(name);
                    }
                }
            }
        }

        params
    }

    fn emit_class_method_with_overloads(name: &str, overloads: &[(Vec<String>, String)]) -> String {
        if overloads.is_empty() {
            return format!("async {}() {{\n}}\n", name);
        }

        if overloads.len() == 1 {
            let (params, body) = &overloads[0];
            let mut out = String::new();
            out.push_str("async ");
            out.push_str(name);
            out.push('(');
            out.push_str(&params.join(", "));
            out.push(')');
            out.push_str(body);
            out.push('\n');
            return out;
        }

        let mut out = String::new();
    out.push_str("async ");
    out.push_str(name);
        out.push_str("(...args) {\n");

        for (idx, (params, body)) in overloads.iter().enumerate() {
            if idx == 0 {
                out.push_str("    if ");
            } else {
                out.push_str("    else if ");
            }
            out.push_str(&format!("(args.length === {})", params.len()));
            out.push_str(" {\n");

            if !params.is_empty() {
                out.push_str("      const [");
                out.push_str(&params.join(", "));
                out.push_str("] = args;\n");
            }

            let body_inner = body
                .trim()
                .trim_start_matches('{')
                .trim_end_matches('}')
                .trim();
            for line in body_inner.lines() {
                let trimmed = line.trim();
                if trimmed.is_empty() {
                    continue;
                }
                out.push_str("      ");
                out.push_str(trimmed);
                out.push('\n');
            }

            out.push_str("    }\n");
        }

        out.push_str("    else {\n");
        out.push_str(&format!(
            "      throw new Error(\"No matching overload for {}\");\n",
            name
        ));
        out.push_str("    }\n");
        out.push_str("  }\n\n");
        out
    }

    fn transpile_companion_object(node: &Node, source: &str) -> String {
        let mut result = String::new();
        let child_count = node.child_count();

        for i in 0..child_count {
            if let Some(child) = node.child(i as u32) {
                if child.kind() != "class_body" {
                    continue;
                }

                for j in 0..child.child_count() {
                    if let Some(member) = child.child(j as u32) {
                        if member.kind() == "property_declaration" {
                            let mut field = Self::transpile_binding_declaration(&member, source);
                            field = field.trim_start_matches("let ").to_string();
                            result.push_str("  static ");
                            result.push_str(&field);
                        }
                    }
                }
            }
        }

        result
    }

    fn transpile_variable(node: &Node, source: &str) -> String {
        Self::transpile_binding_declaration(node, source)
    }

    fn transpile_binding_declaration(node: &Node, source: &str) -> String {
        let name = Self::find_first_identifier_text(node, source);
        let initializer = Self::find_first_initializer(node, source);

        let mut result = String::from("let ");
        result.push_str(name.as_deref().unwrap_or(""));

        if let Some(initializer) = initializer {
            let initializer = Self::normalize_initializer_js(initializer.trim());
            let initializer = Self::normalize_if_expression_initializer(&initializer);
            let initializer = initializer.trim();
            if !initializer.is_empty() && initializer != name.as_deref().unwrap_or("") {
                result.push_str(" = ");
                result.push_str(initializer);
            }
        }

        format!("{};\n", result)
    }

    fn find_first_identifier_text(node: &Node, source: &str) -> Option<String> {
        let child_count = node.child_count();

        for i in 0..child_count {
            if let Some(child) = node.child(i as u32) {
                match child.kind() {
                    "identifier" | "simple_identifier" => {
                        return Some(Self::get_node_text(&child, source));
                    }
                    _ => {
                        if let Some(found) = Self::find_first_identifier_text(&child, source) {
                            return Some(found);
                        }
                    }
                }
            }
        }

        None
    }

    fn normalize_initializer_js(initializer: &str) -> String {
        let apply_marker = ".apply {";
        let build_marker = "}.build()";

        if let Some(apply_idx) = initializer.find(apply_marker) {
            if let Some(build_idx) = initializer.rfind(build_marker) {
                if build_idx > apply_idx {
                    let receiver = initializer[..apply_idx].trim();
                    let body_start = apply_idx + apply_marker.len();
                    let body = initializer[body_start..build_idx].trim();

                    let mut out = String::new();
                    out.push_str("(() => { const __tmp = ");
                    out.push_str(receiver);
                    out.push_str("; ");
                    out.push_str(&Self::transpile_apply_block_body(body, "__tmp").replace('\n', " "));

                    out.push_str("return __tmp.build(); })()");
                    return out;
                }
            }
        }

        initializer.to_string()
    }

    fn normalize_if_expression_initializer(initializer: &str) -> String {
        let trimmed = initializer.trim();
        if !trimmed.starts_with("if (") {
            return trimmed.to_string();
        }

        let cond_open = match trimmed.find('(') {
            Some(pos) => pos,
            None => return trimmed.to_string(),
        };
        let cond_close = match Self::find_matching_paren_end(trimmed, cond_open) {
            Some(pos) => pos,
            None => return trimmed.to_string(),
        };

        let condition = trimmed[cond_open + 1..cond_close - 1].trim();
        let mut rest = trimmed[cond_close..].trim_start();
        if !rest.starts_with('{') {
            return trimmed.to_string();
        }

        let then_end = match Self::find_matching_brace_end(rest, 0) {
            Some(pos) => pos,
            None => return trimmed.to_string(),
        };
        let then_block = &rest[1..then_end - 1];

        rest = rest[then_end..].trim_start();
        if !rest.starts_with("else") {
            return trimmed.to_string();
        }
        rest = rest[4..].trim_start();
        if !rest.starts_with('{') {
            return trimmed.to_string();
        }

        let else_end = match Self::find_matching_brace_end(rest, 0) {
            Some(pos) => pos,
            None => return trimmed.to_string(),
        };
        let else_block = &rest[1..else_end - 1];

        let then_expr = then_block.trim().trim_end_matches(';');
        let else_expr = else_block.trim().trim_end_matches(';');
        if then_expr.is_empty() || else_expr.is_empty() {
            return trimmed.to_string();
        }

        format!("({} ? {} : {})", condition, then_expr, else_expr)
    }

    fn transpile_apply_let_chain(node: &Node, source: &str) -> Option<String> {
        let mut receiver_chain: Option<Node> = None;
        let mut value_arguments: Option<Node> = None;

        for i in 0..node.child_count() {
            if let Some(child) = node.child(i as u32) {
                match child.kind() {
                    "navigation_expression" => {
                        if receiver_chain.is_none() {
                            receiver_chain = Some(child);
                        }
                    }
                    "value_arguments" => {
                        if value_arguments.is_none() {
                            value_arguments = Some(child);
                        }
                    }
                    _ => {}
                }
            }
        }

        let receiver_chain = receiver_chain?;
        let value_arguments = value_arguments?;
        let receiver_chain_text = Self::get_node_text(&receiver_chain, source);
        let apply_marker = ".apply {";
        let apply_idx = receiver_chain_text.find(apply_marker)?;

        let receiver = receiver_chain_text[..apply_idx].trim();
        if receiver.is_empty() {
            return None;
        }

        let body_start = apply_idx + apply_marker.len();
        let body_end = Self::find_matching_brace_end(&receiver_chain_text, body_start - 1)?;
        let body = receiver_chain_text[body_start..body_end - 1].trim();

        let let_target = {
            let mut target = String::new();
            let child_count = value_arguments.child_count();
            for i in 0..child_count {
                if let Some(child) = value_arguments.child(i as u32) {
                    if child.kind() == "value_argument" {
                        let text = Self::transpile_node(&child, source);
                        if !text.trim().is_empty() {
                            target = text.trim().to_string();
                            break;
                        }
                    }
                }
            }
            target
        };

        if let_target.is_empty() {
            return None;
        }

        let receiver_var = "__tmp";
        let mut result = String::from("(() => {\n");
        result.push_str("  const __tmp = ");
        result.push_str(receiver);
        result.push_str(";\n");
        result.push_str(&Self::transpile_apply_block_body(body, receiver_var));
        result.push_str("  ");
        result.push_str(&Self::normalize_kotlin_callable_reference(&let_target));
        result.push_str("(__tmp);\n");
        result.push_str("  return __tmp;\n");
        result.push_str("})()");
        Some(result)
    }

    fn normalize_kotlin_callable_reference(value: &str) -> String {
        value.trim().replace("::", ".")
    }

    fn transpile_apply_block_body(body: &str, receiver_var: &str) -> String {
        let mut result = String::new();

        for line in body.lines() {
            let trimmed = line.trim();
            if trimmed.is_empty() {
                continue;
            }

            if trimmed.starts_with("//") {
                continue;
            }

            let indent = line
                .chars()
                .take_while(|ch| ch.is_whitespace())
                .collect::<String>();

            if let Some((lhs, rhs)) = trimmed.split_once(" = ") {
                let lhs = lhs.trim();
                if !lhs.is_empty() && lhs.chars().all(|ch| ch.is_ascii_alphanumeric() || ch == '_' || ch == '$') {
                    result.push_str("  ");
                    result.push_str(&indent);
                    result.push_str(receiver_var);
                    result.push('.');
                    result.push_str(lhs);
                    result.push_str(" = ");
                    result.push_str(rhs.trim_start());
                    if !rhs.trim_end().ends_with('+') {
                        result.push(';');
                    }
                    result.push('\n');
                    continue;
                }
            }

            if trimmed.starts_with('"') || trimmed.starts_with('\'') || trimmed.starts_with('+') {
                if trimmed == "+" {
                    continue;
                }

                result.push_str("  ");
                result.push_str(&indent);
                result.push_str(trimmed);
                if !trimmed.starts_with('+') && !trimmed.ends_with(';') && !trimmed.ends_with('+') {
                    result.push(';');
                }
                result.push('\n');
                continue;
            }

            let call_candidate = trimmed.strip_suffix(';').unwrap_or(trimmed);
            if call_candidate.contains('(')
                && !call_candidate.starts_with("if ")
                && !call_candidate.starts_with("for ")
                && !call_candidate.starts_with("while ")
                && !call_candidate.starts_with("switch ")
                && !call_candidate.starts_with("return ")
            {
                result.push_str("  ");
                result.push_str(&indent);
                result.push_str(receiver_var);
                if call_candidate.starts_with('.') {
                    result.push_str(call_candidate);
                } else {
                    result.push('.');
                    result.push_str(call_candidate);
                }
                result.push_str(";\n");
                continue;
            }

            result.push_str("  ");
            result.push_str(&indent);
            result.push_str(trimmed);
            if !trimmed.ends_with(';') && !trimmed.ends_with('{') && !trimmed.ends_with('}') {
                result.push(';');
            }
            result.push('\n');
        }

        result
    }

    fn find_matching_brace_end(source: &str, open_brace_index: usize) -> Option<usize> {
        let bytes = source.as_bytes();
        if bytes.get(open_brace_index) != Some(&b'{') {
            return None;
        }

        let mut depth = 0usize;
        let mut i = open_brace_index;
        let mut in_single = false;
        let mut in_double = false;
        let mut in_template = false;
        let mut in_line_comment = false;
        let mut in_block_comment = false;
        let mut escaped = false;

        while i < bytes.len() {
            let b = bytes[i];
            let next = bytes.get(i + 1).copied();

            if in_line_comment {
                if b == b'\n' {
                    in_line_comment = false;
                }
                i += 1;
                continue;
            }

            if in_block_comment {
                if b == b'*' && next == Some(b'/') {
                    in_block_comment = false;
                    i += 2;
                    continue;
                }
                i += 1;
                continue;
            }

            if in_single {
                if escaped {
                    escaped = false;
                } else if b == b'\\' {
                    escaped = true;
                } else if b == b'\'' {
                    in_single = false;
                }
                i += 1;
                continue;
            }

            if in_double {
                if escaped {
                    escaped = false;
                } else if b == b'\\' {
                    escaped = true;
                } else if b == b'"' {
                    in_double = false;
                }
                i += 1;
                continue;
            }

            if in_template {
                if escaped {
                    escaped = false;
                } else if b == b'\\' {
                    escaped = true;
                } else if b == b'`' {
                    in_template = false;
                }
                i += 1;
                continue;
            }

            if b == b'/' && next == Some(b'/') {
                in_line_comment = true;
                i += 2;
                continue;
            }

            if b == b'/' && next == Some(b'*') {
                in_block_comment = true;
                i += 2;
                continue;
            }

            if b == b'\'' {
                in_single = true;
                i += 1;
                continue;
            }

            if b == b'"' {
                in_double = true;
                i += 1;
                continue;
            }

            if b == b'`' {
                in_template = true;
                i += 1;
                continue;
            }

            if b == b'{' {
                depth += 1;
            } else if b == b'}' {
                if depth == 0 {
                    return None;
                }

                depth -= 1;
                if depth == 0 {
                    return Some(i + 1);
                }
            }

            i += 1;
        }

        None
    }

    fn find_first_initializer(node: &Node, source: &str) -> Option<String> {
        let child_count = node.child_count();
        let mut seen_equals = false;

        for i in 0..child_count {
            if let Some(child) = node.child(i as u32) {
                match child.kind() {
                    "=" => {
                        seen_equals = true;
                    }
                    _ if seen_equals && !child.is_extra() => {
                        let text = Self::transpile_node(&child, source);
                        if !text.trim().is_empty() {
                            return Some(text.trim().to_string());
                        }
                    }
                    _ => {}
                }
            }
        }

        None
    }

    fn transpile_call(node: &Node, source: &str) -> String {
        if let Some(js) = Self::transpile_apply_let_chain(node, source) {
            return js;
        }

        let mut result = String::new();
        let child_count = node.child_count();

        for i in 0..child_count {
            if let Some(child) = node.child(i as u32) {
                match child.kind() {
                    "postfix_expression"
                    | "simple_identifier"
                    | "identifier"
                    | "navigation_expression"
                    | "this_expression"
                    | "super_expression" => {
                        result.push_str(&Self::transpile_node(&child, source));
                    }
                    "call_suffix" | "value_arguments" => {
                        result.push_str(&Self::transpile_call_suffix(&child, source));
                    }
                    _ => {}
                }
            }
        }

        if result.trim().is_empty() {
            Self::get_node_text(node, source)
        } else {
            Self::rewrite_kotlin_trailing_lambdas(&result)
        }
    }

    fn transpile_call_suffix(node: &Node, source: &str) -> String {
        let mut result = String::from("(");
        let child_count = node.child_count();
        let mut first_argument = true;

        for i in 0..child_count {
            if let Some(child) = node.child(i as u32) {
                match child.kind() {
                    "(" | ")" | "," => {}
                    "value_argument" => {
                        if !first_argument {
                            result.push_str(", ");
                        }
                        result.push_str(&Self::transpile_node(&child, source));
                        first_argument = false;
                    }
                    _ if !child.is_extra() => result.push_str(&Self::transpile_node(&child, source)),
                    _ => {}
                }
            }
        }

        result.push(')');
        result
    }

    fn transpile_string_literal(node: &Node, source: &str) -> String {
        let raw = Self::get_node_text(node, source);
        if !(raw.starts_with('"') && raw.ends_with('"')) {
            return raw;
        }

        if !raw.contains('$') {
            return raw;
        }

        let inner = &raw[1..raw.len() - 1];
        let mut out = String::from("`");
        let chars: Vec<char> = inner.chars().collect();
        let mut i = 0usize;

        while i < chars.len() {
            let ch = chars[i];
            if ch == '$' {
                if i + 1 < chars.len() && chars[i + 1] == '{' {
                    let mut j = i + 2;
                    let mut expr = String::new();
                    while j < chars.len() && chars[j] != '}' {
                        expr.push(chars[j]);
                        j += 1;
                    }
                    out.push_str("${");
                    out.push_str(expr.trim());
                    out.push('}');
                    i = if j < chars.len() { j + 1 } else { j };
                    continue;
                }

                let mut j = i + 1;
                let mut ident = String::new();
                while j < chars.len() {
                    let c = chars[j];
                    if c.is_ascii_alphanumeric() || c == '_' {
                        ident.push(c);
                        j += 1;
                    } else {
                        break;
                    }
                }

                if ident.is_empty() {
                    out.push('$');
                    i += 1;
                    continue;
                }

                out.push_str("${");
                out.push_str(&ident);
                out.push('}');
                i = j;
                continue;
            }

            if ch == '`' {
                out.push_str("\\`");
            } else {
                out.push(ch);
            }
            i += 1;
        }

        out.push('`');
        out
    }

    fn transpile_lambda(node: &Node, source: &str) -> String {
        let mut result = String::from("(");
        let mut in_params = true;
        let child_count = node.child_count();

        for i in 0..child_count {
            if let Some(child) = node.child(i as u32) {
                match child.kind() {
                    "lambda_parameters" => {
                        // Extract parameters
                        let param_count = child.child_count();
                        for j in 0..param_count {
                            if let Some(param) = child.child(j as u32) {
                                if param.kind() == "variable_declaration" {
                                    result.push_str(&Self::get_node_text(&param, source));
                                    result.push_str(", ");
                                }
                            }
                        }
                    }
                    "->" => {
                        in_params = false;
                        result.push_str(") => ");
                    }
                    _ if !in_params && child.kind() != "block" => {
                        result.push_str(&Self::transpile_node(&child, source));
                    }
                    "block" if !in_params => {
                        result.push_str(&Self::transpile_block(&child, source));
                    }
                    _ => {}
                }
            }
        }

        result
    }

    fn transpile_if(node: &Node, source: &str) -> String {
        let condition = node
            .child_by_field_name("condition")
            .map(|cond| Self::transpile_node(&cond, source))
            .unwrap_or_default();

        if condition.trim().is_empty() {
            return format!("{}\n", Self::get_node_text(node, source));
        }

        let mut non_condition_nodes: Vec<Node> = Vec::new();
        let cond_span = node
            .child_by_field_name("condition")
            .map(|cond| (cond.start_byte(), cond.end_byte()));
        let named_count = node.named_child_count();
        for i in 0..named_count {
            if let Some(child) = node.named_child(i as u32) {
                if let Some((start, end)) = cond_span {
                    if child.start_byte() == start && child.end_byte() == end {
                        continue;
                    }
                }
                non_condition_nodes.push(child);
            }
        }

        let mut result = String::new();
        result.push_str("if (");
        result.push_str(condition.trim());
        result.push_str(") ");

        if let Some(then_node) = non_condition_nodes.first() {
            if then_node.kind() == "block" {
                result.push_str(&Self::transpile_block(then_node, source));
            } else {
                result.push_str("{\n  ");
                result.push_str(&Self::transpile_node(then_node, source));
                result.push_str("\n}\n");
            }
        }

        if let Some(else_node) = non_condition_nodes.get(1) {
            result.push_str(" else ");
            if else_node.kind() == "block" || else_node.kind() == "if_expression" {
                result.push_str(&Self::transpile_node(else_node, source));
            } else {
                result.push_str("{\n  ");
                result.push_str(&Self::transpile_node(else_node, source));
                result.push_str("\n}\n");
            }
        }

        format!("{}\n", result)
    }

    fn transpile_when(node: &Node, source: &str) -> String {
        let child_count = node.child_count();
        let mut subject_expr: Option<String> = None;
        let mut entries: Vec<Node> = Vec::new();

        for i in 0..child_count {
            if let Some(child) = node.child(i as u32) {
                match child.kind() {
                    "when_subject" => {
                        if let Some(expr) = child.child(0) {
                            subject_expr = Some(Self::transpile_node(&expr, source));
                        }
                    }
                    "when_entry" => entries.push(child),
                    _ => {}
                }
            }
        }

        if let Some(subject) = subject_expr {
            let mut result = String::from("(() => { switch (");
            result.push_str(subject.trim());
            result.push_str(") {\n");
            for entry in entries {
                result.push_str(&Self::transpile_when_entry(&entry, source));
            }
            result.push_str("} })()");
            return result;
        }

        let mut result = String::from("(() => { ");
        let mut emitted_any_conditional = false;
        let mut default_body: Option<String> = None;

        for entry in entries {
            let condition: Option<String> = entry
                .child_by_field_name("condition")
                .map(|c| Self::transpile_node(&c, source))
                .or_else(|| {
                    // Named fallback only for explicit condition nodes.
                    let named_count = entry.named_child_count();
                    for idx in 0..named_count {
                        if let Some(named) = entry.named_child(idx as u32) {
                            if named.kind() == "when_condition" {
                                let text = Self::transpile_node(&named, source);
                                if !text.trim().is_empty() {
                                    return Some(text);
                                }
                            }
                        }
                    }
                    None
                });

            let body: Option<String> = entry
                .child_by_field_name("consequence")
                .and_then(|b| {
                    if b.kind() == "line_comment" || b.kind() == "block_comment" {
                        None
                    } else {
                        let text = Self::transpile_node(&b, source);
                        if text.trim().is_empty() {
                            None
                        } else {
                            Some(text)
                        }
                    }
                })
                .or_else(|| {
                    // Fallback: use the last named non-comment child as branch value.
                    let named_count = entry.named_child_count();
                    if named_count == 0 {
                        return None;
                    }

                    for idx in (0..named_count).rev() {
                        if let Some(n) = entry.named_child(idx as u32) {
                            if n.kind() == "line_comment" || n.kind() == "block_comment" {
                                continue;
                            }
                            let text = Self::transpile_node(&n, source);
                            if !text.trim().is_empty() {
                                return Some(text);
                            }
                        }
                    }

                    None
                });

            let body_text = body
                .unwrap_or_else(|| "undefined".to_string())
                .trim()
                .to_string();

            if let Some(cond) = condition {
                if !emitted_any_conditional {
                    result.push_str("if (");
                } else {
                    result.push_str("else if (");
                }
                result.push_str(cond.trim());
                result.push_str(") { return ");
                result.push_str(&body_text);
                result.push_str("; } ");
                emitted_any_conditional = true;
            } else {
                default_body = Some(body_text);
            }
        }

        if let Some(default_value) = default_body {
            if emitted_any_conditional {
                result.push_str("else { return ");
                result.push_str(&default_value);
                result.push_str("; } ");
            } else {
                result.push_str("return ");
                result.push_str(&default_value);
                result.push_str("; ");
            }
        } else if !emitted_any_conditional {
            result.push_str("return undefined; ");
        }

        result.push_str("})()");
        result
    }

    fn transpile_when_entry(node: &Node, source: &str) -> String {
        let mut result = String::new();
        let child_count = node.child_count();
        let mut consequence_expr: Option<String> = None;

        // First pass: collect all conditions and the consequence
        for i in 0..child_count {
            if let Some(child) = node.child(i as u32) {
                match child.kind() {
                    "when_condition" => {
                        result.push_str("case ");
                        result.push_str(&Self::transpile_node(&child, source));
                        result.push_str(":\n");
                    }
                    "->" => {}
                    "block" => {
                        // Extract the body from the block
                        let block_content = Self::transpile_block(&child, source);
                        consequence_expr = Some(block_content);
                    }
                    _ => {
                        // Try to capture simple expression consequences
                        if consequence_expr.is_none() && 
                           !child.kind().contains("comment") && 
                           child.kind() != "LINE_SEPARATOR" &&
                           child.kind() != "NL" {
                            let expr = Self::transpile_node(&child, source).trim().to_string();
                            if !expr.is_empty() && expr != "{" && expr != "}" && !expr.starts_with("/*") {
                                consequence_expr = Some(expr);
                            }
                        }
                    }
                }
            }
        }

        // Add the consequence with return statement
        if let Some(consequence) = consequence_expr {
            result.push_str("return ");
            result.push_str(consequence.trim());
            if !consequence.trim().ends_with(";") {
                result.push(';');
            }
            result.push('\n');
        }

        result
    }

    fn transpile_for(node: &Node, source: &str) -> String {
        let child_count = node.child_count();
        let mut iterator_name = String::new();
        let mut iterable_expr = String::new();
        let mut body = String::from("{\n}\n");

        for i in 0..child_count {
            if let Some(child) = node.child(i as u32) {
                match child.kind() {
                    "variable_declaration" => {
                        iterator_name = Self::find_first_identifier_text(&child, source)
                            .unwrap_or_else(|| Self::transpile_node(&child, source));
                    }
                    "block" => {
                        body = Self::transpile_block(&child, source);
                    }
                    "in" => {}
                    "postfix_expression"
                    | "simple_identifier"
                    | "identifier"
                    | "navigation_expression"
                    | "call_expression"
                        if iterable_expr.is_empty() =>
                    {
                        iterable_expr = Self::transpile_node(&child, source);
                    }
                    _ => {}
                }
            }
        }

        if iterator_name.trim().is_empty() || iterable_expr.trim().is_empty() {
            return format!("{}\n", Self::get_node_text(node, source));
        }

        format!(
            "for (const {} of {}) {}\n",
            iterator_name.trim(),
            iterable_expr.trim(),
            body
        )
    }

    fn transpile_while(node: &Node, source: &str) -> String {
        let condition = node
            .child_by_field_name("condition")
            .map(|cond| Self::transpile_node(&cond, source))
            .unwrap_or_default();

        if condition.trim().is_empty() {
            return format!("{}\n", Self::get_node_text(node, source));
        }

        let mut result = String::new();
        result.push_str("while (");
        result.push_str(condition.trim());
        result.push_str(") ");

        let child_count = node.child_count();
        for i in 0..child_count {
            if let Some(child) = node.child(i as u32) {
                if child.kind() == "block" {
                    result.push_str(&Self::transpile_block(&child, source));
                    break;
                }
            }
        }

        format!("{}\n", result)
    }

    fn transpile_do_while(node: &Node, source: &str) -> String {
        let condition = node
            .child_by_field_name("condition")
            .map(|cond| Self::transpile_node(&cond, source))
            .unwrap_or_default();

        let mut body = String::from("{\n}\n");
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i as u32) {
                if child.kind() == "block" {
                    body = Self::transpile_block(&child, source);
                    break;
                }
            }
        }

        if condition.trim().is_empty() {
            return format!("do {};\n", body.trim());
        }

        format!("do {} while ({});\n", body, condition.trim())
    }

    fn transpile_assignment(node: &Node, source: &str) -> String {
        let mut result = String::new();
        let child_count = node.child_count();

        for i in 0..child_count {
            if let Some(child) = node.child(i as u32) {
                match child.kind() {
                    "=" => {
                        result.push_str(" = ");
                    }
                    _ => {
                        result.push_str(&Self::transpile_node(&child, source));
                    }
                }
            }
        }

        format!("{};\n", result)
    }

    fn transpile_binary(node: &Node, source: &str) -> String {
        let node_text = Self::get_node_text(node, source);
        if node_text.contains("?:") {
            let left = node
                .child_by_field_name("left")
                .map(|n| Self::transpile_node(&n, source))
                .unwrap_or_default();
            let right = node
                .child_by_field_name("right")
                .map(|n| Self::transpile_node(&n, source))
                .unwrap_or_default();

            if right.trim_start().starts_with("throw ") {
                return format!(
                    "(({}) ?? (() => {{ {}; }})())",
                    left.trim(),
                    right.trim().trim_end_matches(';')
                );
            }

            return format!("(({}) ?? ({}))", left.trim(), right.trim());
        }

        let mut result = String::new();
        let child_count = node.child_count();

        for i in 0..child_count {
            if let Some(child) = node.child(i as u32) {
                match child.kind() {
                    "+" | "-" | "*" | "/" | "%" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "&&"
                    | "||" | "&" | "|" | "^" | "?:" => {
                        result.push(' ');
                        let op = Self::get_node_text(&child, source);
                        if op == "?:" {
                            result.push_str("??");
                        } else {
                            result.push_str(&op);
                        }
                        result.push(' ');
                    }
                    "as" => {
                        // Type cast in Kotlin, ignore in JS
                    }
                    _ if !child.is_extra() => {
                        result.push_str(&Self::transpile_node(&child, source));
                    }
                    _ => {}
                }
            }
        }

        result
    }

    fn transpile_postfix(node: &Node, source: &str) -> String {
        if let Some(js) = Self::transpile_apply_let_chain(node, source) {
            return js;
        }

        let mut result = String::new();
        let child_count = node.child_count();

        for i in 0..child_count {
            if let Some(child) = node.child(i as u32) {
                match child.kind() {
                    "postfix_suffix" => {
                        let suffix = Self::get_node_text(&child, source);
                        if suffix.trim() == "!!" {
                            continue;
                        }
                        result.push_str(&suffix);
                    }
                    _ => {
                        result.push_str(&Self::transpile_node(&child, source));
                    }
                }
            }
        }

        result
    }

    fn transpile_prefix(node: &Node, source: &str) -> String {
        let mut result = String::new();
        let child_count = node.child_count();

        for i in 0..child_count {
            if let Some(child) = node.child(i as u32) {
                result.push_str(&Self::transpile_node(&child, source));
            }
        }

        result
    }

    fn transpile_block(node: &Node, source: &str) -> String {
        let mut result = String::from(" {\n");
        let child_count = node.child_count();

        for i in 0..child_count {
            if let Some(child) = node.child(i as u32) {
                if child.kind() != "{" && child.kind() != "}" && !child.is_extra() {
                    let child_output = Self::transpile_node(&child, source);
                    if !child_output.trim().is_empty() {
                        let trimmed_output = child_output.trim_end();

                        if let Some(flattened) = Self::flatten_apply_iife_statement(trimmed_output) {
                            for line in flattened.lines() {
                                let trimmed_line = line.trim();
                                if trimmed_line.is_empty() {
                                    continue;
                                }
                                result.push_str("  ");
                                result.push_str(trimmed_line);
                                if !trimmed_line.ends_with(';')
                                    && !trimmed_line.ends_with('}')
                                    && !trimmed_line.ends_with('{')
                                    && !trimmed_line.ends_with(':')
                                    && !trimmed_line.ends_with('+')
                                {
                                    result.push(';');
                                }
                                result.push('\n');
                            }
                            continue;
                        }

                        result.push_str("  ");
                        result.push_str(trimmed_output);
                        if !trimmed_output.ends_with(';')
                            && !trimmed_output.ends_with('}')
                            && !trimmed_output.ends_with('{')
                            && !trimmed_output.ends_with(':')
                            && !trimmed_output.ends_with('+')
                        {
                            result.push(';');
                        }
                        result.push('\n');
                    }
                }
            }
        }

        result.push_str("}\n");
        result
    }

    fn get_node_text(node: &Node, source: &str) -> String {
        let start = node.start_byte();
        let end = node.end_byte();
        if end <= source.len() {
            source[start..end].to_string()
        } else {
            String::new()
        }
    }

    fn find_matching_paren_end(source: &str, open_paren_index: usize) -> Option<usize> {
        let bytes = source.as_bytes();
        if bytes.get(open_paren_index) != Some(&b'(') {
            return None;
        }

        let mut depth = 0usize;
        let mut i = open_paren_index;
        while i < bytes.len() {
            let b = bytes[i];
            if b == b'(' {
                depth += 1;
            } else if b == b')' {
                if depth == 0 {
                    return None;
                }
                depth -= 1;
                if depth == 0 {
                    return Some(i + 1);
                }
            }
            i += 1;
        }

        None
    }

    fn sanitize_js_text(text: &str) -> String {
        let text = text.replace("!!", "");
        let text = Self::rewrite_kotlin_collection_calls(&text);
        Self::rewrite_runtime_kotlinisms(&text)
    }

    fn sanitize_output_js(text: &str) -> String {
        let text = text.replace("return throw ", "throw ");
        let text = Self::rewrite_kotlin_collection_calls(&text);
        let text = Self::rewrite_kotlin_return_control_flow(&text);
        let text = Self::rewrite_kotlin_for_ranges(&text);
        let text = Self::rewrite_kotlin_catch_clauses(&text);
        let text = Self::rewrite_kotlin_trailing_lambdas(&text);
        let text = Self::rewrite_runtime_kotlinisms(&text);
        let text = Self::rewrite_extension_calls(&text);
        let text = Self::inject_extension_helpers(&text);
        let text = Self::rewrite_kotlin_string_helpers(&text);
        let text = Self::rewrite_collection_helpers(&text);
        let text = Self::rewrite_instance_scoping(&text);
        let text = Self::rewrite_async_bridge(&text);
        let text = Self::rewrite_kotlin_null_safety(&text);
        Self::rewrite_exports_metadata(&text)
    }

    fn rewrite_collection_helpers(text: &str) -> String {
        let filter_is_instance = Regex::new(
            r"\.filterIsInstance<\s*([A-Za-z_][A-Za-z0-9_\.]*)\s*>\(\)",
        )
        .expect("valid filterIsInstance regex");
        let out = filter_is_instance
            .replace_all(text, ".filter((x) => x instanceof $1)")
            .to_string();

        let bare_map = Regex::new(r"\b([A-Za-z_][A-Za-z0-9_\.]*)\.map\s*;")
            .expect("valid bare map regex");
        bare_map
            .replace_all(&out, "$1.map((it) => it);")
            .to_string()
    }

    fn rewrite_instance_scoping(text: &str) -> String {
        let out = text.replace("${baseUrl}", "${this.baseUrl}");

        let preferences = Regex::new(r"\bpreferences\.")
            .expect("valid preferences scope regex");
        let out = preferences.replace_all(&out, "this.preferences.").to_string();

        let get_headers = Regex::new(r"\bGET\(([^,]+),\s*headers\)")
            .expect("valid GET headers regex");
        let out = get_headers
            .replace_all(&out, "GET($1, this.headers)")
            .to_string();

        let static_fields = Regex::new(
            r"=\s*(PREF_[A-Z0-9_]+|NSFW_[A-Z0-9_]+|DEDUPLICATE_CHAPTERS|ALTERNATIVE_NAMES_IN_DESCRIPTION)\b",
        )
        .expect("valid static field regex");
        static_fields
            .replace_all(&out, "= this.constructor.$1")
            .to_string()
    }

    fn rewrite_async_bridge(text: &str) -> String {
        let mut out = text.to_string();

        let exec_parse = Regex::new(r"=\s*([^;]*?)\.execute\(\)\.parseAs\(\)\s*;")
            .expect("valid execute parse regex");
        out = exec_parse
            .replace_all(&out, "= parseAs(await $1.execute());")
            .to_string();

        let return_get = Regex::new(r"\breturn\s+GET\(")
            .expect("valid return GET regex");
        out = return_get.replace_all(&out, "return await GET(").to_string();

        let assign_get = Regex::new(r"=\s*GET\(")
            .expect("valid assign GET regex");
        out = assign_get.replace_all(&out, "= await GET(").to_string();

        let call_execute = Regex::new(r"=\s*([^;]*?\.execute\(\))\s*;")
            .expect("valid execute await regex");
        out = call_execute.replace_all(&out, "= await $1;").to_string();

        out
    }

    fn rewrite_exports_metadata(text: &str) -> String {
        let class_re = Regex::new(r"\bclass\s+([A-Za-z_][A-Za-z0-9_]*)")
            .expect("valid class capture regex");

        let mut out = text.to_string();
        if let Some(caps) = class_re.captures(text) {
            if !text.contains("export default class") {
                if let Some(m) = caps.get(0) {
                    out = out.replacen(m.as_str(), &format!("export default {}", m.as_str()), 1);
                }
            }

            if let Some(name_match) = caps.get(1) {
                let class_name = name_match.as_str();
                if !out.contains("export const __transpilerMeta") {
                    out.push_str("\n");
                    out.push_str("export const __transpilerMeta = {\n");
                    out.push_str(&format!("  className: \"{}\",\n", class_name));
                    out.push_str("  moduleFormat: \"esm\",\n");
                    out.push_str("};\n");
                }
            }
        }

        out
    }

    fn rewrite_extension_calls(text: &str) -> String {
        let to_http_url = Regex::new(r"\b([A-Za-z_][A-Za-z0-9_\.]*)\.toHttpUrl\(\)")
            .expect("valid toHttpUrl extension regex");
        let out = to_http_url
            .replace_all(text, "toHttpUrl_ext($1)")
            .to_string();

        let parse_as = Regex::new(r"\b([A-Za-z_][A-Za-z0-9_\.]*)\.parseAs\(\)")
            .expect("valid parseAs extension regex");
        parse_as.replace_all(&out, "parseAs_ext($1)").to_string()
    }

    fn inject_extension_helpers(text: &str) -> String {
        let mut helpers = String::new();

        if text.contains("toHttpUrl_ext(") && !text.contains("function toHttpUrl_ext(") {
            helpers.push_str("function toHttpUrl_ext(receiver) { return toHttpUrl(receiver); }\n");
        }

        if text.contains("parseAs_ext(") && !text.contains("function parseAs_ext(") {
            helpers.push_str("function parseAs_ext(receiver) { return parseAs(receiver); }\n");
        }

        if helpers.is_empty() {
            return text.to_string();
        }

        format!("{}{}", helpers, text)
    }

    fn rewrite_kotlin_string_helpers(text: &str) -> String {
        let is_not_blank = Regex::new(r"\b([A-Za-z_][A-Za-z0-9_\.]*)\.isNotBlank\(\)")
            .expect("valid isNotBlank regex");
        let out = is_not_blank
            .replace_all(text, "($1 && $1.trim().length > 0)")
            .to_string();

        let remove_prefix = Regex::new(r#"\b([A-Za-z_][A-Za-z0-9_\.]*)\.removePrefix\(\"/\"\)"#)
            .expect("valid removePrefix regex");
        let out = remove_prefix
            .replace_all(&out, "(($1).startsWith(\"/\") ? ($1).slice(1) : ($1))")
            .to_string();

        let substring_after_last = Regex::new(
            r#"\b([A-Za-z_][A-Za-z0-9_\.]*)\.substringAfterLast\(\"/\"\)"#,
        )
        .expect("valid substringAfterLast regex");
        substring_after_last
            .replace_all(&out, "(($1).slice(($1).lastIndexOf(\"/\") + 1))")
            .to_string()
    }

    fn rewrite_kotlin_null_safety(text: &str) -> String {
        let elvis_return = Regex::new(
            r"\b([A-Za-z_][A-Za-z0-9_\.]*)\s*\?\?\s*\(return\s+([^;]+?)\)",
        )
        .expect("valid elvis return regex");
        let text = elvis_return
            .replace_all(text, "(() => { const __v = $1; if (__v == null) return $2; return __v; })()")
            .to_string();

        let elvis_throw = Regex::new(
            r"\b([A-Za-z_][A-Za-z0-9_\.]*)\s*\?\?\s*\(\(\)\s*=>\s*\{\s*throw\s+([^;]+);\s*\}\)\(\)",
        )
        .expect("valid elvis throw regex");

        elvis_throw
            .replace_all(&text, "(() => { const __v = $1; if (__v == null) { throw $2; } return __v; })()")
            .to_string()
    }

    fn rewrite_kotlin_for_ranges(text: &str) -> String {
        let range_for = Regex::new(
            r"for\s*\(\s*([A-Za-z_][A-Za-z0-9_]*)\s+in\s+([^\{]+?)\s+until\s+([^\{]+?)\s*\)\s*\{",
        )
        .expect("valid Kotlin range for regex");

        range_for
            .replace_all(text, "for (let $1 = $2; $1 < $3; $1++) {")
            .to_string()
    }

    fn rewrite_kotlin_catch_clauses(text: &str) -> String {
        let catch_clause = Regex::new(r"catch\s*\(\s*([A-Za-z_][A-Za-z0-9_]*)\s*:\s*[A-Za-z_][A-Za-z0-9_\.<>?]*\s*\)")
            .expect("valid Kotlin catch regex");
        catch_clause.replace_all(text, "catch ($1)").to_string()
    }

    fn rewrite_kotlin_return_control_flow(text: &str) -> String {
        let return_if = Regex::new(
            r"return\s+if\s*\((?s:.*?)\)\s*\{\s*(?s:.*?)\s*\}\s*else\s*\{\s*(?s:.*?)\s*\}",
        )
        .expect("valid return-if regex");
        let out = return_if
            .replace_all(text, |caps: &regex::Captures<'_>| {
                let full = caps.get(0).map(|m| m.as_str()).unwrap_or_default();
                let if_start = full.find("if").unwrap_or(0);
                let if_text = &full[if_start..];
                let normalized = Self::normalize_if_expression_initializer(if_text)
                    .trim()
                    .to_string();
                format!("return {};", normalized)
            })
            .to_string();

        let return_try = Regex::new(
            r"return\s+try\s*\{\s*(?s:(.*?))\s*\}\s*catch\s*\(\s*([A-Za-z_][A-Za-z0-9_]*)\s*(?::\s*[^)]*)?\)\s*\{\s*(?s:(.*?))\s*\}",
        )
        .expect("valid return-try regex");
        return_try
            .replace_all(&out, "return (() => { try { return $1; } catch ($2) { return $3; } })();")
            .to_string()
    }

    fn rewrite_return_if_expression(expr: &str) -> Option<String> {
        let trimmed = expr.trim_start();
        if !trimmed.starts_with("if (") {
            return None;
        }

        let rewritten = Self::normalize_if_expression_initializer(trimmed);
        if rewritten == trimmed {
            return None;
        }

        Some(format!("return {};", rewritten))
    }

    fn rewrite_return_try_expression(expr: &str) -> Option<(String, usize)> {
        let trimmed = expr.trim_start();
        if !trimmed.starts_with("try") {
            return None;
        }

        let try_open = trimmed.find('{')?;
        let try_close = Self::find_matching_brace_end(trimmed, try_open)?;
        let try_body = trimmed[try_open + 1..try_close - 1].trim();

        let after_try = trimmed[try_close..].trim_start();
        if !after_try.starts_with("catch") {
            return None;
        }

        let catch_paren_open = after_try.find('(')?;
        let catch_paren_close = Self::find_matching_paren_end(after_try, catch_paren_open)?;
        let catch_param = after_try[catch_paren_open + 1..catch_paren_close - 1].trim();

        let after_catch_paren = after_try[catch_paren_close..].trim_start();
        if !after_catch_paren.starts_with('{') {
            return None;
        }

        let catch_open = after_catch_paren.find('{')?;
        let catch_close = Self::find_matching_brace_end(after_catch_paren, catch_open)?;
        let catch_body = after_catch_paren[catch_open + 1..catch_close - 1].trim();

        let try_body = Self::rewrite_kotlin_return_control_flow(try_body);
        let catch_body = Self::rewrite_kotlin_return_control_flow(catch_body);

        let rewritten = format!(
            "return (() => {{ try {{ return {}; }} catch ({}) {{ return {}; }} }})();",
            try_body,
            catch_param,
            catch_body
        );

        Some((rewritten, catch_close + (after_try.len() - after_catch_paren.len())))
    }

    fn flatten_apply_iife_statement(text: &str) -> Option<String> {
        let trimmed = text.trim();
        if !(trimmed.starts_with("(() => {") && trimmed.ends_with("})()")) {
            return None;
        }

        let body = trimmed
            .trim_start_matches("(() => {")
            .trim_end_matches("})()")
            .trim();

        if !body.contains("const __tmp =") {
            return None;
        }

        let mut out_lines: Vec<String> = Vec::new();
        out_lines.push("{".to_string());
        for line in body.lines() {
            let t = line.trim();
            if t.is_empty() || t == "return __tmp;" {
                continue;
            }
            out_lines.push(t.to_string());
        }
        out_lines.push("}".to_string());

        if out_lines.len() <= 2 {
            None
        } else {
            Some(out_lines.join("\n"))
        }
    }

    fn rewrite_kotlin_collection_calls(text: &str) -> String {
        let int_array_lambda = Regex::new(
            r"IntArray\(([^\)]+)\)\s*\{\s*([^{}]+?)\s*\}",
        )
        .expect("valid IntArray lambda regex");
        let text = int_array_lambda
            .replace_all(text, "Array.from({ length: $1 }, (_, it) => $2)")
            .to_string();

        let byte_array_lambda = Regex::new(
            r"ByteArray\(([^\)]+)\)\s*\{\s*([^{}]+?)\s*\}",
        )
        .expect("valid ByteArray lambda regex");
        let text = byte_array_lambda
            .replace_all(&text, "Array.from({ length: $1 }, (_, it) => $2)")
            .to_string();

        let mut out = String::new();
        let mut i = 0usize;

        while i < text.len() {
            let rest = match text.get(i..) {
                Some(r) => r,
                None => break,
            };

            if rest.starts_with("listOf(") {
                let open = i + "listOf".len();
                if let Some(end) = Self::find_matching_paren_end(&text, open) {
                    let inner = &text[open + 1..end - 1];
                    out.push('[');
                    out.push_str(inner);
                    out.push(']');
                    i = end;
                    continue;
                }
            }

            if rest.starts_with("arrayOf(") {
                let open = i + "arrayOf".len();
                if let Some(end) = Self::find_matching_paren_end(&text, open) {
                    let inner = &text[open + 1..end - 1];
                    out.push('[');
                    out.push_str(inner);
                    out.push(']');
                    i = end;
                    continue;
                }
            }

            if rest.starts_with("ArrayList<") {
                if let Some(gt_rel) = rest.find('>') {
                    let after_gt = i + gt_rel + 1;
                    if text.get(after_gt..after_gt + 2) == Some("()") {
                        out.push_str("[]");
                        i = after_gt + 2;
                        continue;
                    }
                }
            }

            if rest.starts_with("ArrayList(") {
                let open = i + "ArrayList".len();
                if let Some(end) = Self::find_matching_paren_end(&text, open) {
                    let inner = text[open + 1..end - 1].trim();
                    if inner.is_empty() {
                        out.push_str("[]");
                        i = end;
                        continue;
                    }

                    out.push_str("[...");
                    out.push_str(inner);
                    out.push(']');
                    i = end;
                    continue;
                }
            }

            if let Some(ch) = rest.chars().next() {
                out.push(ch);
                i += ch.len_utf8();
            } else {
                break;
            }
        }

        out
    }

    fn rewrite_runtime_kotlinisms(text: &str) -> String {
        let text = text
            .replace(".addAll(", ".push(...")
            .replace(".isEmpty()", ".length === 0")
            .replace(".mapIndexed(", ".map(")
            .replace("LinkedHashMap()", "new Map()")
            .replace("throw Exception(", "throw new Error(")
            .replace(" ushr ", " >>> ")
            .replace(" shl ", " << ")
            .replace(" shr ", " >> ")
            .replace(" xor ", " ^ ")
            .replace(" and ", " & ")
            .replace(" or ", " | ");

        Self::rewrite_map_semantics(&text)
    }

    fn rewrite_map_semantics(text: &str) -> String {
        let map_decl = Regex::new(
            r"(?:\blet\s+|\b)([A-Za-z_][A-Za-z0-9_]*)\s*=\s*new\s+Map\(\)\s*;",
        )
        .expect("valid map declaration regex");

        let mut out = text.to_string();
        let map_vars: Vec<String> = map_decl
            .captures_iter(text)
            .filter_map(|caps| caps.get(1).map(|m| m.as_str().to_string()))
            .collect();

        for map_var in map_vars {
            let escaped = regex::escape(&map_var);

            let read_pattern = Regex::new(&format!(
                r"\blet\s+([A-Za-z_][A-Za-z0-9_]*)\s*=\s*{}\s*\[\s*([^\]]+?)\s*\]\s*;",
                escaped
            ))
            .expect("valid map read regex");
            out = read_pattern
                .replace_all(&out, format!("let $1 = {}.get($2);", map_var).as_str())
                .to_string();

            let write_pattern = Regex::new(&format!(
                r"\b{}\s*\[\s*([^\]]+?)\s*\]\s*=\s*([^;]+);",
                escaped
            ))
            .expect("valid map write regex");
            out = write_pattern
                .replace_all(&out, format!("{}.set($1, $2);", map_var).as_str())
                .to_string();

            let values_pattern = Regex::new(&format!(
                r"\b{}\.values\.toList\(\)",
                escaped
            ))
            .expect("valid map values regex");
            out = values_pattern
                .replace_all(&out, format!("Array.from({}.values())", map_var).as_str())
                .to_string();
        }

        out
    }

    fn rewrite_kotlin_trailing_lambdas(text: &str) -> String {
        let methods = ["forEach", "mapIndexed", "map"];
        let mut out = String::new();
        let mut cursor = 0usize;

        while let Some(rel_dot) = text[cursor..].find('.') {
            let dot = cursor + rel_dot;
            out.push_str(&text[cursor..dot]);

            let mut replaced = false;
            for method in methods {
                let name_start = dot + 1;
                let name_end = name_start + method.len();
                if name_end > text.len() || !text[name_start..].starts_with(method) {
                    continue;
                }

                let brace_start = Self::skip_ascii_whitespace(text, name_end);
                if text.as_bytes().get(brace_start) != Some(&b'{') {
                    continue;
                }

                if let Some(brace_end) = Self::find_matching_brace_end(text, brace_start) {
                    let body = text[brace_start + 1..brace_end - 1].trim();
                    out.push('.');
                    out.push_str(method);
                    out.push_str(&Self::build_js_lambda_call(method, body));
                    cursor = brace_end;
                    replaced = true;
                    break;
                }
            }

            if !replaced {
                out.push('.');
                cursor = dot + 1;
            }
        }

        out.push_str(&text[cursor..]);
        out
    }

    fn skip_ascii_whitespace(text: &str, mut index: usize) -> usize {
        let bytes = text.as_bytes();
        while let Some(b) = bytes.get(index) {
            if !b.is_ascii_whitespace() {
                break;
            }
            index += 1;
        }
        index
    }

    fn build_js_lambda_call(method: &str, kotlin_body: &str) -> String {
        let (mut params, body) = match kotlin_body.split_once("->") {
            Some((raw_params, raw_body)) => {
                let params: Vec<String> = raw_params
                    .split(',')
                    .map(|p| p.trim())
                    .filter(|p| !p.is_empty())
                    .map(|p| p.to_string())
                    .collect();
                (params, raw_body.trim().to_string())
            }
            None => (vec!["it".to_string()], kotlin_body.trim().to_string()),
        };

        if method == "mapIndexed" && params.len() >= 2 {
            // Kotlin mapIndexed lambda params are (index, item), JS map callback is (item, index).
            params.swap(0, 1);
        }

        if params.is_empty() {
            params.push("it".to_string());
        }

        let params = params.join(", ");
        let body = Self::strip_kotlin_named_args(&Self::sanitize_js_text(body.trim()));
        let body = body.trim();

        if method == "forEach" {
            return format!("(({}) => {{ {} }})", params, Self::ensure_statement(body));
        }

        if Self::looks_like_expression(body) {
            format!("(({}) => {})", params, body)
        } else {
            format!("(({}) => {{ {} }})", params, Self::ensure_statement(body))
        }
    }

    fn strip_kotlin_named_args(value: &str) -> String {
        let bytes = value.as_bytes();
        let mut out = String::with_capacity(value.len());
        let mut i = 0usize;
        let mut paren_depth = 0usize;

        while i < bytes.len() {
            let b = bytes[i];

            if b == b'(' {
                paren_depth += 1;
                out.push('(');
                i += 1;
                continue;
            }

            if b == b')' {
                paren_depth = paren_depth.saturating_sub(1);
                out.push(')');
                i += 1;
                continue;
            }

            if paren_depth > 0
                && (b.is_ascii_alphabetic() || b == b'_')
                && Self::is_named_arg_boundary(bytes, i)
            {
                let mut ident_end = i + 1;
                while ident_end < bytes.len()
                    && (bytes[ident_end].is_ascii_alphanumeric() || bytes[ident_end] == b'_')
                {
                    ident_end += 1;
                }

                let mut eq_pos = ident_end;
                while eq_pos < bytes.len() && bytes[eq_pos].is_ascii_whitespace() {
                    eq_pos += 1;
                }

                if eq_pos < bytes.len()
                    && bytes[eq_pos] == b'='
                    && bytes.get(eq_pos + 1) != Some(&b'=')
                    && bytes.get(eq_pos + 1) != Some(&b'>')
                {
                    i = eq_pos + 1;
                    while i < bytes.len() && bytes[i].is_ascii_whitespace() {
                        i += 1;
                    }
                    continue;
                }
            }

            out.push(b as char);
            i += 1;
        }

        out
    }

    fn is_named_arg_boundary(bytes: &[u8], index: usize) -> bool {
        if index == 0 {
            return false;
        }

        let mut i = index;
        while i > 0 {
            i -= 1;
            if bytes[i].is_ascii_whitespace() {
                continue;
            }
            return bytes[i] == b'(' || bytes[i] == b',';
        }

        false
    }

    fn looks_like_expression(value: &str) -> bool {
        !value.is_empty()
            && !value.contains('\n')
            && !value.contains(';')
            && !value.starts_with("if ")
            && !value.starts_with("for ")
            && !value.starts_with("while ")
            && !value.starts_with("switch ")
            && !value.starts_with("return ")
    }

    fn ensure_statement(value: &str) -> String {
        let trimmed = value.trim();
        if trimmed.is_empty() {
            return String::new();
        }
        if trimmed.ends_with(';') || trimmed.ends_with('}') {
            trimmed.to_string()
        } else {
            format!("{};", trimmed)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Deref;

    use super::*;
    use crate::{FULL_TEST_SOURCE_CODE, parse_kotlin_code};
    use oxc_allocator::Allocator;
    use oxc_codegen::Codegen;
    use oxc_parser::Parser;
    use oxc_span::SourceType;

    fn try_format_with_oxc(js_code: &str) -> Option<String> {
        let allocator = Allocator::default();
        let parsed = Parser::new(&allocator, js_code, SourceType::mjs()).parse();
        
        if !parsed.errors.is_empty() {
            println!("Oxc parse errors:");
            for error in parsed.errors.iter() {
                println!("  {:?}", error);

                let labels = error
                    .deref()
                    .labels
                    .as_ref()
                    .map(|labels| labels.as_slice())
                    .unwrap_or(&[]);

                for label in labels {
                    let offset = label.offset();
                    let length = label.len();
                    let clamped_offset = offset.min(js_code.len());
                    let line_start = js_code[..clamped_offset]
                        .rfind('\n')
                        .map(|idx| idx + 1)
                        .unwrap_or(0);
                    let line_end = js_code[clamped_offset..]
                        .find('\n')
                        .map(|idx| clamped_offset + idx)
                        .unwrap_or(js_code.len());
                    let line_number = js_code[..clamped_offset]
                        .bytes()
                        .filter(|byte| *byte == b'\n')
                        .count()
                        + 1;
                    let column_number = js_code[line_start..clamped_offset].chars().count() + 1;
                    let line_text = &js_code[line_start..line_end];
                    let caret_width = length.max(1);

                    println!("    --> line {}, column {}", line_number, column_number);
                    println!("     |");
                    println!("{:>4} | {}", line_number, line_text);
                    println!("     | {:>width$}", "^".repeat(caret_width), width = column_number + caret_width - 1);
                }
            }
            return None;
        } 

        if parsed.panicked {
            println!("Oxc parser panicked while parsing full program.");
            return  None;  
        } 

        Some(Codegen::new().build(&parsed.program).code)
    }

    #[test]
    fn test_simple_function_transpile() {
        let kotlin_code = r#"
fun main() {
    println("Hello, World!")
}
"#;
        let tree = parse_kotlin_code(kotlin_code);
        println!("AST:\n{}", tree.root_node().to_sexp());
        
        let js_code = KotlinTranspiler::transpile(kotlin_code, &tree);
        let formatted_js = try_format_with_oxc(&js_code).unwrap();
        
        // println!("Transpiled JS:\n{}", js_code);
        println!("Formatted JS:\n{}", formatted_js);
        
        assert!(!js_code.is_empty());
    }

    #[test]
    fn test_variable_declaration() {
        let kotlin_code = r#"
fun test() {
    val x = 5
    var y = 10
}
"#;
        let tree = parse_kotlin_code(kotlin_code);
        let js_code = KotlinTranspiler::transpile(kotlin_code, &tree);
        let formatted_js = try_format_with_oxc(&js_code).unwrap();

        println!("Transpiled JS:\n{}", js_code);
        println!("Formatted JS:\n{}", formatted_js);

        assert!(js_code.contains("let"));
    }

    #[test]
    fn test_simple_expression_transpile() {
        let kotlin_code = r#"
fun test() {
    val x = 5 + 3 * 2 - 4 / 2
}
"#;
        let tree = parse_kotlin_code(kotlin_code);
        let js_code = KotlinTranspiler::transpile(kotlin_code, &tree);
        let formatted_js = try_format_with_oxc(&js_code).unwrap();

        // println!("Transpiled JS:\n{}", js_code);
        println!("Formatted JS:\n{}", formatted_js);

        assert!(js_code.contains("let"));
    }

    #[test]
    fn test_apply_let_chain_transpile() {
        let kotlin_code = r#"
fun setupPreferenceScreen(screen: PreferenceScreen) {
    ListPreference(screen.context).apply {
        key = PREF_POSTER_QUALITY
        title = "Thumbnail Quality"
        setDefaultValue("large")
    }.let(screen::addPreference)
}
"#;
        let tree = parse_kotlin_code(kotlin_code);
        println!("AST:\n{}", tree.root_node().to_sexp());
        let js_code = KotlinTranspiler::transpile(kotlin_code, &tree);
        let formatted_js = try_format_with_oxc(&js_code).unwrap();

        // println!("Transpiled JS:\n{}", js_code);
        println!("Formatted JS:\n{}", formatted_js);

        assert!(!js_code.contains(".apply {"));
        assert!(!js_code.contains(".let("));
        assert!(formatted_js.contains("screen.addPreference") || formatted_js.contains("addPreference"));
    }

    #[test]
    fn test_import_transpile() {
        let kotlin_code = r#"
import java.util.List
import kotlin.collections.Map
"#;
        let tree = parse_kotlin_code(kotlin_code);
        let js_code = KotlinTranspiler::transpile(kotlin_code, &tree);
        let formatted_js = try_format_with_oxc(&js_code).unwrap();

        // println!("Transpiled JS:\n{}", js_code);
        println!("Formatted JS:\n{}", formatted_js);
        assert!(js_code.contains("import { List } from \"java.util\";"));
    }

    #[test]
    fn full_test() {
        let kotlin_code = FULL_TEST_SOURCE_CODE;
        let tree = parse_kotlin_code(kotlin_code);
        let js_code = KotlinTranspiler::transpile(kotlin_code, &tree);
        let formatted_js = try_format_with_oxc(&js_code).unwrap();

        // println!("Transpiled JS:\n{}", js_code);
        println!("Formatted JS:\n{}", formatted_js);
        assert!(!js_code.is_empty());
    }

    #[test]
    fn full_files_test() {
        // get files in temp/files with .kt extension
        // get current pwd
        let current_dir = std::env::current_dir().expect("Failed to get current directory");
        let full_abs_path = current_dir.join("./temp/files");
        let kotline_files = std::fs::read_dir(&full_abs_path)
            .expect(format!("Failed to read {}, at {}", full_abs_path.display(), full_abs_path.display()).as_str())
            .filter_map(|entry| {
                let entry = entry.ok()?;
                let path = entry.path();
                if path.extension()?.to_str()? == "kt" {
                    Some(path)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        let mut errored_count = 0;
        for file_path in &kotline_files {
            println!("Testing file: {}", file_path.display());
            let kotlin_code = std::fs::read_to_string(&file_path)
                .unwrap_or_else(|_| panic!("Failed to read file {}", file_path.display()));
            let tree = parse_kotlin_code(&kotlin_code);
            let js_code = KotlinTranspiler::transpile(&kotlin_code, &tree);

            if try_format_with_oxc(&js_code).is_none() {
                println!("Oxc failed to parse the generated JS for file {}", file_path.display());
                println!("Transpiled JS for file {}:\n{}", file_path.display(), js_code);
                errored_count += 1;
            }
        }
        println!("Total errors found: {}/{}", errored_count, kotline_files.len());  
    }
}
