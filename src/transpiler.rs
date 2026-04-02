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
        let raw_js = Self::normalize_kotlin_apply_let_chains(&Self::transpile_node(&root, kotlin_code));
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
            "navigation_expression" => Self::get_node_text(node, source),
            "lambda_literal" => Self::transpile_lambda(node, source),
            "if_expression" => Self::transpile_if(node, source),
            "when_expression" => Self::transpile_when(node, source),
            "for_statement" => Self::transpile_for(node, source),
            "while_statement" => Self::transpile_while(node, source),
            "assignment" => Self::transpile_assignment(node, source),
            "binary_expression" => Self::transpile_binary(node, source),
            "postfix_expression" => Self::transpile_postfix(node, source),
            "prefix_expression" => Self::transpile_prefix(node, source),
            "block" => Self::transpile_block(node, source),
            _ => Self::get_node_text(node, source),
        }
    }

    fn transpile_value_argument(node: &Node, source: &str) -> String {
        let child_count = node.child_count();
        for i in 0..child_count {
            if let Some(child) = node.child(i as u32) {
                if child.is_extra() || child.kind() == "," {
                    continue;
                }
                let out = Self::transpile_node(&child, source);
                if !out.trim().is_empty() {
                    return out;
                }
            }
        }
        String::new()
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
        let mut result = String::from("function ");
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

        if let Some(class_name) = Self::find_first_identifier_text(node, source) {
            result.push_str(&class_name);
        }

        for i in 0..node.child_count() {
            if let Some(child) = node.child(i as u32) {
                match child.kind() {
                    "class_body" | "block" => {
                        result.push_str(&Self::transpile_block(&child, source));
                    }
                    _ => {}
                }
            }
        }

        format!("{}\n", result)
    }

    fn transpile_property(node: &Node, source: &str) -> String {
        Self::transpile_binding_declaration(node, source)
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

                    if !body.is_empty() {
                        out.push_str("/* Kotlin apply block omitted in JS transpile: ");
                        let single_line = body.replace('\n', " ").replace("*/", "* /");
                        out.push_str(single_line.trim());
                        out.push_str(" */ ");
                    }

                    out.push_str("return __tmp.build(); })()");
                    return out;
                }
            }
        }

        initializer.to_string()
    }

    fn transpile_apply_let_chain(node: &Node, source: &str) -> Option<String> {
        let raw = Self::get_node_text(node, source);
        let apply_marker = ".apply {";
        let let_marker = "}.let(";

        let apply_idx = raw.find(apply_marker)?;
        let let_idx = raw.rfind(let_marker)?;
        if let_idx <= apply_idx {
            return None;
        }

        let receiver = raw[..apply_idx].trim();
        if receiver.is_empty() {
            return None;
        }

        let body_start = apply_idx + apply_marker.len();
        let body_end = Self::find_matching_brace_end(&raw, body_start - 1)?;
        if body_end > let_idx {
            return None;
        }

        let body = raw[body_start..body_end - 1].trim();
        let let_call_open = let_idx + let_marker.len() - 1;
        let let_call_end = Self::find_matching_paren_end(&raw, let_call_open)?;
        if let_call_end <= let_call_open + 1 {
            return None;
        }

        let let_target = raw[let_call_open + 1..let_call_end - 1].trim();
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
        result.push_str(&Self::normalize_kotlin_callable_reference(let_target));
        result.push_str("(__tmp);\n");
        result.push_str("  return __tmp;\n");
        result.push_str("})()");
        Some(result)
    }

    fn normalize_kotlin_apply_let_chains(source: &str) -> String {
        let mut output = String::new();
        let mut cursor = 0usize;

        while let Some(rel_apply_idx) = source[cursor..].find(".apply {") {
            let apply_idx = cursor + rel_apply_idx;
            let let_idx = match source[apply_idx..].find("}.let(") {
                Some(rel_let_idx) => apply_idx + rel_let_idx,
                None => break,
            };

            let body_start = apply_idx + ".apply {".len();
            let body_end = match Self::find_matching_brace_end(source, body_start - 1) {
                Some(end) => end,
                None => break,
            };

            if body_end > let_idx {
                break;
            }

            let let_call_open = let_idx + ".let(".len() - 1;
            let let_call_end = match Self::find_matching_paren_end(source, let_call_open) {
                Some(end) => end,
                None => break,
            };

            let receiver_slice = &source[cursor..apply_idx];
            let receiver = receiver_slice
                .trim_end()
                .rsplit_once('\n')
                .map(|(_, tail)| tail.trim_start())
                .unwrap_or(receiver_slice)
                .trim();
            if receiver.is_empty() {
                output.push_str(&source[cursor..let_call_end]);
                cursor = let_call_end;
                continue;
            }

            let receiver_start = receiver_slice.rfind(receiver).map(|idx| cursor + idx).unwrap_or(apply_idx);
            output.push_str(&source[cursor..receiver_start]);

            let body = source[body_start..body_end - 1].trim();
            let let_target = source[let_call_open + 1..let_call_end - 1].trim();
            if let_target.is_empty() {
                output.push_str(&source[receiver_start..let_call_end]);
                cursor = let_call_end;
                continue;
            }

            output.push_str("(() => {\n");
            output.push_str("  const __tmp = ");
            output.push_str(receiver);
            output.push_str(";\n");
            output.push_str(&Self::transpile_apply_block_body(body, "__tmp"));
            output.push_str("  ");
            output.push_str(&Self::normalize_kotlin_callable_reference(let_target));
            output.push_str("(__tmp);\n");
            output.push_str("  return __tmp;\n");
            output.push_str("})()");

            cursor = let_call_end;
        }

        output.push_str(&source[cursor..]);
        output
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
                    result.push('\n');
                    continue;
                }
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
                result.push('.');
                result.push_str(call_candidate);
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

    fn find_matching_paren_end(source: &str, open_paren_index: usize) -> Option<usize> {
        let bytes = source.as_bytes();
        if bytes.get(open_paren_index) != Some(&b'(') {
            return None;
        }

        let mut depth = 0usize;
        let mut i = open_paren_index;
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
            result
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
        Self::get_node_text(node, source)
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

        let mut result = String::new();
        result.push_str("if (");
        result.push_str(condition.trim());
        result.push_str(") ");

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
            let mut result = String::from("switch (");
            result.push_str(subject.trim());
            result.push_str(") {\n");
            for entry in entries {
                result.push_str(&Self::transpile_when_entry(&entry, source));
            }
            result.push_str("}\n");
            return result;
        }

        // Kotlin `when { ... }` (no subject) behaves like if/else-if expression.
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

        for i in 0..child_count {
            if let Some(child) = node.child(i as u32) {
                match child.kind() {
                    "when_condition" => {
                        result.push_str("case ");
                        result.push_str(&Self::transpile_node(&child, source));
                        result.push_str(":");
                    }
                    "->" => {}
                    "block" => {
                        result.push_str(&Self::transpile_block(&child, source));
                        result.push_str("break;");
                    }
                    _ => {}
                }
            }
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
        let mut result = String::new();
        let child_count = node.child_count();

        for i in 0..child_count {
            if let Some(child) = node.child(i as u32) {
                match child.kind() {
                    "+" | "-" | "*" | "/" | "%" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "&&"
                    | "||" | "&" | "|" | "^" => {
                        result.push(' ');
                        result.push_str(&Self::get_node_text(&child, source));
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
                        result.push_str(&Self::get_node_text(&child, source));
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
                        result.push_str("  ");
                        result.push_str(&child_output);
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{FULL_TEST_SOURCE_CODE, parse_kotlin_code};
    use oxc_allocator::Allocator;
    use oxc_codegen::Codegen;
    use oxc_parser::Parser;
    use oxc_span::SourceType;

    fn try_format_with_oxc(js_code: &str) -> Option<String> {
        let allocator = Allocator::default();
        let parsed = Parser::new(&allocator, js_code, SourceType::mjs()).parse();

        if parsed.panicked || !parsed.errors.is_empty() {
            return None;
        }

        Some(Codegen::new().build(&parsed.program).code)
    }

    fn format_js_with_oxc(js_code: &str) -> String {
        if let Some(formatted) = try_format_with_oxc(js_code) {
            return formatted;
        }

        println!("Oxc full-program parse failed; applying best-effort function-level formatting.");
        format_functions_with_oxc(js_code)
    }

    fn format_functions_with_oxc(js_code: &str) -> String {
        let mut output = String::new();
        let mut cursor = 0usize;

        while let Some(rel_start) = js_code[cursor..].find("function ") {
            let start = cursor + rel_start;
            output.push_str(&js_code[cursor..start]);

            let remaining = &js_code[start..];
            if let Some(fn_len) = extract_function_len(remaining) {
                let function_text = &remaining[..fn_len];
                if let Some(formatted_fn) = try_format_with_oxc(function_text) {
                    output.push_str(formatted_fn.trim_end());
                } else {
                    output.push_str(function_text);
                }
                cursor = start + fn_len;
            } else {
                output.push_str(remaining);
                cursor = js_code.len();
            }
        }

        if cursor < js_code.len() {
            output.push_str(&js_code[cursor..]);
        }

        output
    }

    fn extract_function_len(source: &str) -> Option<usize> {
        let bytes = source.as_bytes();
        let open_brace = bytes.iter().position(|b| *b == b'{')?;

        let mut depth = 0usize;
        let mut i = open_brace;
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
        let formatted_js = format_js_with_oxc(&js_code);
        
        println!("Transpiled JS:\n{}", js_code);
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
        let formatted_js = format_js_with_oxc(&js_code);

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
        let formatted_js = format_js_with_oxc(&js_code);

        println!("Transpiled JS:\n{}", js_code);
        println!("Formatted JS:\n{}", formatted_js);

        assert!(js_code.contains("let"));
    }

    #[test]
    fn test_import_transpile() {
        let kotlin_code = r#"
import java.util.List
import kotlin.collections.Map
"#;
        let tree = parse_kotlin_code(kotlin_code);
        let js_code = KotlinTranspiler::transpile(kotlin_code, &tree);
        let formatted_js = format_js_with_oxc(&js_code);

        println!("Transpiled JS:\n{}", js_code);
        println!("Formatted JS:\n{}", formatted_js);
        assert!(js_code.contains("import { List } from \"java.util\";"));
    }

    #[test]
    fn full_test() {
        let kotlin_code = FULL_TEST_SOURCE_CODE;
        let tree = parse_kotlin_code(kotlin_code);
        let js_code = KotlinTranspiler::transpile(kotlin_code, &tree);
        let formatted_js = format_js_with_oxc(&js_code);

        println!("Transpiled JS:\n{}", js_code);
        println!("Formatted JS:\n{}", formatted_js);
        assert!(!js_code.is_empty());
    }
}
