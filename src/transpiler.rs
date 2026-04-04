use oxc_allocator::Allocator;
use oxc_ast::ast::Program;
use oxc_codegen::Codegen;
use oxc_parser::Parser;
use oxc_span::SourceType;
use std::collections::{HashMap, HashSet};
use tree_sitter::Node;

/// Main transpiler struct that converts Kotlin to JavaScript.
pub struct KotlinTranspiler;

impl KotlinTranspiler {
    /// Transpile Kotlin code to JavaScript with a direct AST pipeline:
    /// tree-sitter Kotlin AST -> custom JS AST -> JavaScript source.
    pub fn transpile(kotlin_code: &str, tree: &tree_sitter::Tree) -> String {
        let js_program = JsProgram::from_kotlin_tree(tree, kotlin_code);
        let raw_js = js_program.emit();

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
}

#[derive(Debug, Default, Clone)]
struct JsProgram {
    body: Vec<JsStmt>,
}

#[derive(Debug, Clone)]
enum JsStmt {
    Import(String),
    Function(JsFunction),
    Class(JsClass),
    VarDecl {
        name: String,
        init: Option<JsExpr>,
    },
    If {
        condition: JsExpr,
        then_body: Vec<JsStmt>,
        else_body: Option<Vec<JsStmt>>,
    },
    Expr(JsExpr),
    Return(Option<JsExpr>),
    Unsupported(String),
}

#[derive(Debug, Clone)]
struct JsFunction {
    name: String,
    params: Vec<String>,
    body: Vec<JsStmt>,
    is_async: bool,
}

#[derive(Debug, Clone)]
struct JsClass {
    name: String,
    members: Vec<JsClassMember>,
}

#[derive(Debug, Clone)]
enum JsClassMember {
    Field {
        name: String,
        init: Option<JsExpr>,
        is_static: bool,
    },
    Method(JsFunction),
}

#[derive(Debug, Clone)]
enum JsExpr {
    Ident(String),
    Number(String),
    String(String),
    Raw(String),
    Call {
        callee: Box<JsExpr>,
        args: Vec<JsExpr>,
    },
    Member {
        object: Box<JsExpr>,
        member: String,
    },
    Index {
        object: Box<JsExpr>,
        index: Box<JsExpr>,
    },
    Binary {
        left: Box<JsExpr>,
        op: String,
        right: Box<JsExpr>,
    },
    Conditional {
        condition: Box<JsExpr>,
        then_expr: Box<JsExpr>,
        else_expr: Box<JsExpr>,
    },
    Lambda {
        params: Vec<String>,
        body: Box<JsExpr>,
    },
    Template {
        parts: Vec<String>,
        expressions: Vec<JsExpr>,
    },
    Unsupported(String),
}

impl JsProgram {
    fn from_kotlin_tree(tree: &tree_sitter::Tree, source: &str) -> Self {
        let root = tree.root_node();
        let mut body = Vec::new();

        for i in 0..root.child_count() {
            if let Some(child) = root.child(i as u32) {
                if child.is_extra() || child.kind() == "ERROR" {
                    continue;
                }

                if let Some(stmt) = lower_top_level_node(&child, source) {
                    body.push(stmt);
                }
            }
        }

        Self { body }
    }

    fn emit(&self) -> String {
        let mut out = String::new();
        for stmt in &self.body {
            emit_stmt(stmt, 0, &mut out);
        }
        out
    }
}

fn lower_top_level_node(node: &Node, source: &str) -> Option<JsStmt> {
    match node.kind() {
        "package_header" | "line_comment" | "block_comment" => None,
        "import" | "import_header" => Some(JsStmt::Import(rewrite_import(&node_text(node, source)))),
        "function_declaration" => Some(JsStmt::Function(lower_function(node, source))),
        "class_declaration" => Some(JsStmt::Class(lower_class(node, source))),
        "property_declaration" | "variable_declaration" => Some(lower_var_decl(node, source)),
        _ => Some(lower_statement(node, source)),
    }
}

fn lower_function(node: &Node, source: &str) -> JsFunction {
    let mut name = String::from("anonymous");
    let mut params = Vec::new();
    let mut body = Vec::new();

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i as u32) {
            match child.kind() {
                "identifier" | "simple_identifier" if name == "anonymous" => {
                    name = sanitize_identifier(&node_text(&child, source));
                }
                "function_value_parameters" => {
                    params = lower_parameters(&child, source);
                }
                "function_body" => {
                    body = lower_function_body(&child, source);
                }
                _ => {}
            }
        }
    }

    JsFunction {
        name,
        params,
        body,
        is_async: true,
    }
}

fn lower_parameters(node: &Node, source: &str) -> Vec<String> {
    let mut params = Vec::new();

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i as u32) {
            if child.kind() == "function_value_parameter" || child.kind() == "parameter" {
                if let Some(name) = find_first_identifier_text(&child, source) {
                    params.push(sanitize_identifier(&name));
                }
            }
        }
    }

    params
}

fn lower_function_body(node: &Node, source: &str) -> Vec<JsStmt> {
    for i in 0..node.child_count() {
        if let Some(child) = node.child(i as u32) {
            if child.kind() == "block" {
                return lower_block(&child, source);
            }
        }
    }

    // Expression-body fallback: fun f() = expr
    for i in 0..node.named_child_count() {
        if let Some(child) = node.named_child(i as u32) {
            if child.kind() != "block" {
                let expr = lower_expr(&child, source);
                if matches!(&expr, JsExpr::Raw(raw) if raw.trim_start().starts_with("throw ")) {
                    return vec![JsStmt::Expr(expr)];
                }

                return vec![JsStmt::Return(Some(expr))];
            }
        }
    }

    let body_text = node_text(node, source);
    let expr = body_text.trim().trim_start_matches('=').trim().to_string();
    if expr.is_empty() {
        Vec::new()
    } else if expr.starts_with("throw ") {
        vec![JsStmt::Expr(JsExpr::Raw(expr))]
    } else if expr.starts_with("throw") {
        vec![JsStmt::Expr(JsExpr::Raw(expr))]
    } else {
        vec![JsStmt::Return(Some(JsExpr::Unsupported(expr)))]
    }
}

fn lower_class(node: &Node, source: &str) -> JsClass {
    let mut name = String::from("AnonymousClass");
    let mut members = Vec::new();

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i as u32) {
            match child.kind() {
                "identifier" | "simple_identifier" if name == "AnonymousClass" => {
                    name = sanitize_identifier(&node_text(&child, source));
                }
                "class_body" => {
                    members = lower_class_members(&child, source);
                }
                _ => {}
            }
        }
    }

    JsClass { name, members }
}

fn lower_class_members(node: &Node, source: &str) -> Vec<JsClassMember> {
    let mut members = Vec::new();

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i as u32) {
            if child.is_extra() || child.kind() == "{" || child.kind() == "}" {
                continue;
            }

            match child.kind() {
                "function_declaration" => {
                    members.push(JsClassMember::Method(lower_function(&child, source)));
                }
                "property_declaration" | "variable_declaration" => {
                    let (name, init) = lower_property_parts(&child, source);
                    members.push(JsClassMember::Field {
                        name,
                        init,
                        is_static: false,
                    });
                }
                "companion_object" | "object_declaration" => {
                    let text = node_text(&child, source);
                    if text.contains("companion object") {
                        let mut static_fields = Vec::new();
                        collect_companion_properties(&child, source, &mut static_fields);
                        for (name, init) in static_fields {
                            members.push(JsClassMember::Field {
                                name,
                                init,
                                is_static: true,
                            });
                        }
                    }
                }
                _ => {}
            }
        }
    }

    members
}

fn collect_companion_properties(node: &Node, source: &str, out: &mut Vec<(String, Option<JsExpr>)>) {
    if node.kind() == "property_declaration" || node.kind() == "variable_declaration" {
        out.push(lower_property_parts(node, source));
        return;
    }

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i as u32) {
            if child.is_extra() {
                continue;
            }
            collect_companion_properties(&child, source, out);
        }
    }
}

fn lower_block(node: &Node, source: &str) -> Vec<JsStmt> {
    let mut body = Vec::new();

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i as u32) {
            if child.is_extra() || child.kind() == "{" || child.kind() == "}" {
                continue;
            }
            body.push(lower_statement(&child, source));
        }
    }

    body
}

fn lower_statement(node: &Node, source: &str) -> JsStmt {
    match node.kind() {
        "property_declaration" | "variable_declaration" => lower_var_decl(node, source),
        "assignment" => lower_assignment_statement(node, source),
        "for_statement" => lower_for_statement(node, source),
        "do_statement" | "do_while_statement" => lower_do_statement(node, source),
        "jump_expression" | "return_expression" => lower_return_statement(node, source),
        "if_expression" => lower_if_statement(node, source),
        "line_comment" | "block_comment" => JsStmt::Unsupported(String::new()),
        _ => JsStmt::Expr(lower_expr(node, source)),
    }
}

fn lower_for_statement(node: &Node, source: &str) -> JsStmt {
    let text = node_text(node, source);
    let header = text
        .split_once('{')
        .map(|(head, _)| head.trim().to_string())
        .unwrap_or_else(|| text.trim().to_string());

    let iter_text = header
        .strip_prefix("for")
        .unwrap_or(&header)
        .trim()
        .trim_start_matches('(')
        .trim_end_matches(')')
        .trim()
        .to_string();

    let (loop_var, iter_expr) = if let Some((lhs, rhs)) = iter_text.split_once(" in ") {
        (lhs.trim().trim_start_matches("val ").trim_start_matches("var ").trim(), rhs.trim())
    } else {
        ("item", iter_text.as_str())
    };

    let mut body = Vec::new();
    for i in 0..node.child_count() {
        if let Some(child) = node.child(i as u32) {
            if child.kind() == "block" {
                body = lower_block(&child, source);
                break;
            }
        }
    }

    let mut out = String::new();
    out.push_str(&format!(
        "for (const {} of {}) {{\n",
        sanitize_identifier(loop_var),
        emit_expr(&lower_expr_text(iter_expr)),
    ));
    for stmt in body {
        emit_stmt(&stmt, 1, &mut out);
    }
    out.push_str("}");

    JsStmt::Expr(JsExpr::Raw(out))
}

fn lower_assignment_statement(node: &Node, source: &str) -> JsStmt {
    let text = node_text(node, source);
    if let Some((left, right)) = text.split_once('=') {
        let left = left.trim();
        let right = right.trim();
        return JsStmt::Expr(JsExpr::Raw(format!("{} = {}", left, emit_expr(&lower_expr_text(right)))));
    }

    JsStmt::Unsupported(text)
}

fn lower_do_statement(node: &Node, source: &str) -> JsStmt {
    let mut body = Vec::new();
    let mut condition: Option<JsExpr> = None;

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i as u32) {
            if child.is_extra() {
                continue;
            }

            match child.kind() {
                "block" => body = lower_block(&child, source),
                "condition" => {
                    if let Some(expr_child) = first_named_child(&child) {
                        condition = Some(lower_expr(&expr_child, source));
                    }
                }
                _ => {}
            }
        }
    }

    let cond = condition
        .map(|expr| emit_expr(&expr))
        .unwrap_or_else(|| "false".to_string());

    let mut out = String::from("do {\n");
    for stmt in body {
        emit_stmt(&stmt, 1, &mut out);
    }
    out.push_str("} while (");
    out.push_str(&cond);
    out.push_str(");");

    JsStmt::Expr(JsExpr::Raw(out))
}

fn lower_navigation_expression(node: &Node, source: &str) -> JsExpr {
    // navigation_expression: receiver . member_access
    // Handles: obj.field, obj.method(), obj.method().chainedCall()
    // Also handles: obj.method { lambda }
    let mut receiver: Option<JsExpr> = None;
    let mut pending_method: Option<String> = None;

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i as u32) {
            match child.kind() {
                "." | "?." => {
                    // Skip the dot separator
                    continue;
                }
                "annotated_lambda" => {
                    // Trailing lambda: method { ... }
                    // Extract lambda from inside annotated_lambda
                    if let Some(lambda_child) = first_named_child(&child) {
                        let lambda_expr = if lambda_child.kind() == "lambda_literal" {
                            lower_lambda_expression(&lambda_child, source)
                        } else if lambda_child.kind() == "lambda_expression" {
                            lower_lambda_expression(&lambda_child, source)
                        } else {
                            JsExpr::Unsupported(node_text(&lambda_child, source))
                        };
                        
                        // Apply lambda as argument to pending method
                        if let Some(method_name) = pending_method.take() {
                            let method_expr = if let Some(recv) = receiver.take() {
                                JsExpr::Member {
                                    object: Box::new(recv),
                                    member: method_name,
                                }
                            } else {
                                JsExpr::Ident(method_name)
                            };
                            
                            receiver = Some(JsExpr::Call {
                                callee: Box::new(method_expr),
                                args: vec![lambda_expr],
                            });
                        }
                    }
                }
                "call_suffix" => {
                    // Handle method call - extract arguments and lambdas
                    let mut args = lower_call_args(&child, source);
                    
                    // Also check for lambda blocks in the call_suffix
                    for j in 0..child.child_count() {
                        if let Some(arg_child) = child.child(j as u32) {
                            if arg_child.kind() == "lambda_expression" {
                                args.push(lower_lambda_expression(&arg_child, source));
                            }
                        }
                    }
                    
                    // If we have a pending method name, create a call expression
                    if let Some(method_name) = pending_method.take() {
                        let method_expr = if let Some(recv) = receiver.take() {
                            JsExpr::Member {
                                object: Box::new(recv),
                                member: method_name,
                            }
                        } else {
                            JsExpr::Ident(method_name)
                        };
                        
                        receiver = Some(JsExpr::Call {
                            callee: Box::new(method_expr),
                            args,
                        });
                    }
                }
                "postfix_expression" | "navigation_expression" | "call_expression" 
                | "identifier" | "simple_identifier" | "this_expression" | "super_expression" => {
                    if receiver.is_none() {
                        receiver = Some(lower_expr(&child, source));
                    } else {
                        // This is a method name
                        if let JsExpr::Ident(name) = lower_expr(&child, source) {
                            pending_method = Some(name);
                        }
                    }
                }
                _ => {
                    if receiver.is_none() {
                        receiver = Some(lower_expr(&child, source));
                    } else if pending_method.is_none() {
                        let expr = lower_expr(&child, source);
                        if let JsExpr::Ident(name) = expr {
                            pending_method = Some(name);
                        }
                    }
                }
            }
        }
    }

    // Handle remaining pending method
    if let Some(method_name) = pending_method.take() {
        if let Some(current_receiver) = receiver.take() {
            JsExpr::Member {
                object: Box::new(current_receiver),
                member: method_name,
            }
        } else {
            JsExpr::Ident(method_name)
        }
    } else if let Some(current_receiver) = receiver {
        current_receiver
    } else {
        lower_expr_text(&node_text(node, source))
    }
}


fn lower_postfix_expression(node: &Node, source: &str) -> JsExpr {
    // postfix_expression: expression postfix_operation*
    // Handles: obj[index], obj!!, obj?.method()
    let mut expr: Option<JsExpr> = None;

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i as u32) {
            match child.kind() {
                "identifier" | "simple_identifier" | "call_expression" | "navigation_expression" => {
                    if expr.is_none() {
                        expr = Some(lower_expr(&child, source));
                    }
                }
                "postfix_suffix" => {
                    // Handle [index], !!., ?., etc.
                    if let Some(current_expr) = expr.take() {
                        expr = Some(lower_postfix_suffix(&child, current_expr, source));
                    }
                }
                "index_suffix" => {
                    if let Some(current_expr) = expr.take() {
                        // Extract the index expression
                        for j in 0..child.child_count() {
                            if let Some(idx_child) = child.child(j as u32) {
                                if idx_child.kind() != "[" && idx_child.kind() != "]" && !idx_child.is_extra() {
                                    let index_expr = lower_expr(&idx_child, source);
                                    expr = Some(JsExpr::Index {
                                        object: Box::new(current_expr),
                                        index: Box::new(index_expr),
                                    });
                                    break;
                                }
                            }
                        }
                    }
                }
                _ => {}
            }
        }
    }

    expr.unwrap_or_else(|| lower_expr_text(&node_text(node, source)))
}

fn lower_postfix_suffix(node: &Node, expr: JsExpr, source: &str) -> JsExpr {
    let text = node_text(node, source);
    let trimmed = text.trim();
    
    if trimmed == "!!" {
        // Non-null assertion - in JS we just return the expr as-is
        expr
    } else if trimmed == "?" {
        // Optional access - JS doesn't have this, return as-is
        expr
    } else if trimmed.starts_with('[') && trimmed.ends_with(']') {
        // Index access [...]
        let index_text = trimmed.strip_prefix('[').unwrap_or("").strip_suffix(']').unwrap_or("");
        JsExpr::Index {
            object: Box::new(expr),
            index: Box::new(JsExpr::Raw(index_text.to_string())),
        }
    } else if trimmed.contains("(") {
        // Method call suffix like .substringAfterLast("/")
        // Extract method name and arguments
        if let Some(paren_pos) = trimmed.find('(') {
            let method_name = trimmed[..paren_pos].trim();
            let args_str = trimmed[paren_pos + 1..trimmed.len() - 1].trim();
            
            // Parse single string argument
            let args = if args_str.is_empty() {
                vec![]
            } else {
                vec![JsExpr::String(args_str.to_string())]
            };
            
            JsExpr::Call {
                callee: Box::new(JsExpr::Member {
                    object: Box::new(expr),
                    member: method_name.to_string(),
                }),
                args,
            }
        } else {
            expr
        }
    } else {
        expr
    }
}


fn lower_string_template(node: &Node, source: &str) -> JsExpr {
    // string_template with interpolation: "hello $name" or "hello ${expr}"
    let mut parts = Vec::new();
    let mut expressions = Vec::new();
    let mut current_part = String::new();

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i as u32) {
            match child.kind() {
                "string_start" | "string_end" | "string_mid" => {
                    let text = node_text(&child, source);
                    // Remove quotes and escape sequences
                    let clean = text
                        .trim_matches('"').trim_matches('\'').trim_matches('`')
                        .replace("\\\"", "\"")
                        .replace("\\'", "'")
                        .replace("\\n", "\n");
                    current_part.push_str(&clean);
                }
                "template_expression" => {
                    // Save current part and start new expression
                    parts.push(current_part.clone());
                    current_part.clear();
                    
                    // Extract expression from ${...}
                    for j in 0..child.child_count() {
                        if let Some(expr_child) = child.child(j as u32) {
                            if expr_child.kind() != "$" && expr_child.kind() != "{" && expr_child.kind() != "}" && !expr_child.is_extra() {
                                expressions.push(lower_expr(&expr_child, source));
                                break;
                            }
                        }
                    }
                }
                _ => {}
            }
        }
    }

    // Push final part
    if !current_part.is_empty() || !expressions.is_empty() {
        parts.push(current_part);
    }

    if parts.is_empty() && expressions.is_empty() {
        // Fallback: extract the whole thing as a raw string template
        JsExpr::Raw(format!("`{}`", node_text(node, source)))
    } else {
        JsExpr::Template { parts, expressions }
    }
}

fn lower_lambda_expression(node: &Node, source: &str) -> JsExpr {
    // lambda_expression: { params -> body } or { body } (implicit it parameter)
    // lambda_literal: { body } (same as lambda_expression but different node type)
    let mut params = Vec::new();
    let mut body_exprs: Vec<JsExpr> = Vec::new();
    let mut has_explicit_params = false;

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i as u32) {
            match child.kind() {
                "lambda_parameters" => {
                    has_explicit_params = true;
                    // Extract parameter names
                    for j in 0..child.child_count() {
                        if let Some(param_child) = child.child(j as u32) {
                            if param_child.kind() == "variable_declaration" || param_child.kind() == "parameter" {
                                if let Some(name) = find_first_identifier_text(&param_child, source) {
                                    params.push(sanitize_identifier(&name));
                                }
                            } else if param_child.kind() == "identifier" || param_child.kind() == "simple_identifier" {
                                params.push(sanitize_identifier(&node_text(&param_child, source)));
                            }
                        }
                    }
                }
                "lambda_body" => {
                    // Extract body expression(s)
                    for j in 0..child.child_count() {
                        if let Some(body_child) = child.child(j as u32) {
                            if !body_child.is_extra() {
                                let mut expr = lower_expr(&body_child, source);
                                if !has_explicit_params {
                                    expr = qualify_lambda_receiver_calls(expr);
                                }
                                body_exprs.push(expr);
                            }
                        }
                    }
                }
                // Handle lambda body that's just a block (no explicit lambda_body node)
                "{" | "}" | "->" => {
                    // Skip braces and arrow
                    continue;
                }
                // For lambda_literal, the body might be a direct expression/statement child
                "block" | "call_expression" | "binary_expression" | "if_expression" 
                | "when_expression" | "navigation_expression" | "postfix_expression"
                | "assignment" | "property_declaration" | "for_statement" | "while_statement" => {
                    let mut expr = lower_expr(&child, source);
                    if !has_explicit_params {
                        expr = qualify_lambda_receiver_calls(expr);
                    }
                    body_exprs.push(expr);
                }
                _ => {
                    // For simple expressions that aren't caught by the above
                    if !child.is_extra() && child.kind() != "{" && child.kind() != "}" {
                        let mut expr = lower_expr(&child, source);
                        if !has_explicit_params {
                            expr = qualify_lambda_receiver_calls(expr);
                        }
                        body_exprs.push(expr);
                    }
                }
            }
        }
    }

    // Combine multiple statements: single expression stays expression-bodied;
    // multiple expressions become a valid JS block-bodied lambda.
    let body = if body_exprs.is_empty() {
        JsExpr::Raw("undefined".to_string())
    } else if body_exprs.len() == 1 {
        body_exprs.into_iter().next().unwrap()
    } else {
        let mut block = String::from("{ ");
        for (idx, expr) in body_exprs.iter().enumerate() {
            let rendered = emit_expr(expr);
            if idx + 1 == body_exprs.len() {
                let statement_like = matches!(expr, JsExpr::Raw(raw) if raw.trim_start().starts_with("if ") || raw.trim_start().starts_with("if("));
                if statement_like {
                    block.push_str(&rendered);
                    block.push_str(" return it; ");
                } else {
                    block.push_str("return ");
                    if matches!(expr, JsExpr::Unsupported(_)) {
                        block.push_str("it");
                    } else {
                        block.push_str(&rendered);
                    }
                    block.push_str("; ");
                }
            } else {
                block.push_str(&rendered);
                block.push_str("; ");
            }
        }
        block.push('}');
        JsExpr::Raw(block)
    };
    
    // For simple lambdas with no parameters, use implicit it
    if params.is_empty() {
        JsExpr::Lambda {
            params: vec!["it".to_string()],
            body: Box::new(body),
        }
    } else {
        JsExpr::Lambda {
            params,
            body: Box::new(body),
        }
    }
}

fn qualify_lambda_receiver_calls(expr: JsExpr) -> JsExpr {
    match expr {
        JsExpr::Raw(raw) => JsExpr::Raw(qualify_raw_receiver_calls(&raw)),
        JsExpr::Call { callee, args } => {
            let qualified_callee = match *callee {
                JsExpr::Ident(name) => JsExpr::Member {
                    object: Box::new(JsExpr::Ident("it".to_string())),
                    member: name,
                },
                other => qualify_lambda_receiver_calls(other),
            };

            JsExpr::Call {
                callee: Box::new(qualified_callee),
                args: args.into_iter().map(qualify_lambda_receiver_calls).collect(),
            }
        }
        JsExpr::Binary { left, op, right } => JsExpr::Binary {
            left: Box::new(qualify_lambda_receiver_calls(*left)),
            op,
            right: Box::new(qualify_lambda_receiver_calls(*right)),
        },
        JsExpr::Conditional {
            condition,
            then_expr,
            else_expr,
        } => JsExpr::Conditional {
            condition: Box::new(qualify_lambda_receiver_calls(*condition)),
            then_expr: Box::new(qualify_lambda_receiver_calls(*then_expr)),
            else_expr: Box::new(qualify_lambda_receiver_calls(*else_expr)),
        },
        JsExpr::Index { object, index } => JsExpr::Index {
            object: Box::new(qualify_lambda_receiver_calls(*object)),
            index: Box::new(qualify_lambda_receiver_calls(*index)),
        },
        JsExpr::Member { object, member } => JsExpr::Member {
            object: Box::new(qualify_lambda_receiver_calls(*object)),
            member,
        },
        JsExpr::Lambda { params, body } => JsExpr::Lambda {
            params,
            body: Box::new(qualify_lambda_receiver_calls(*body)),
        },
        JsExpr::Template { parts, expressions } => JsExpr::Template {
            parts,
            expressions: expressions
                .into_iter()
                .map(qualify_lambda_receiver_calls)
                .collect(),
        },
        other => other,
    }
}

fn qualify_raw_receiver_calls(raw: &str) -> String {
    let chars = raw.chars().collect::<Vec<_>>();
    let mut out = String::with_capacity(raw.len() + 16);
    let mut i = 0;
    let mut in_string: Option<char> = None;
    let mut escape = false;

    while i < chars.len() {
        let ch = chars[i];

        if let Some(quote) = in_string {
            out.push(ch);
            if escape {
                escape = false;
            } else if ch == '\\' {
                escape = true;
            } else if ch == quote {
                in_string = None;
            }
            i += 1;
            continue;
        }

        if ch == '"' || ch == '\'' || ch == '`' {
            in_string = Some(ch);
            out.push(ch);
            i += 1;
            continue;
        }

        if is_ident_start(ch) {
            let start = i;
            let mut j = i + 1;
            while j < chars.len() && is_ident_part(chars[j]) {
                j += 1;
            }

            let ident = chars[start..j].iter().collect::<String>();
            let prev = previous_non_whitespace(&chars, start);
            let next = next_non_whitespace(&chars, j);

            let should_qualify = next == Some('(')
                && prev != Some('.')
                && !is_js_keyword(&ident)
                && !is_known_unqualified_fn(&ident)
                && ident != "it"
                && !ident
                    .chars()
                    .next()
                    .map(|c| c.is_ascii_uppercase())
                    .unwrap_or(false);

            if should_qualify {
                out.push_str("it.");
            }
            out.push_str(&ident);
            i = j;
            continue;
        }

        out.push(ch);
        i += 1;
    }

    out
}

fn previous_non_whitespace(chars: &[char], mut idx: usize) -> Option<char> {
    while idx > 0 {
        idx -= 1;
        let ch = chars[idx];
        if !ch.is_whitespace() {
            return Some(ch);
        }
    }
    None
}

fn next_non_whitespace(chars: &[char], mut idx: usize) -> Option<char> {
    while idx < chars.len() {
        let ch = chars[idx];
        if !ch.is_whitespace() {
            return Some(ch);
        }
        idx += 1;
    }
    None
}

fn is_ident_start(ch: char) -> bool {
    ch == '_' || ch == '$' || ch.is_ascii_alphabetic()
}

fn is_ident_part(ch: char) -> bool {
    is_ident_start(ch) || ch.is_ascii_digit()
}

fn is_js_keyword(value: &str) -> bool {
    matches!(
        value,
        "if"
            | "else"
            | "for"
            | "while"
            | "switch"
            | "case"
            | "break"
            | "continue"
            | "return"
            | "throw"
            | "new"
            | "typeof"
            | "delete"
            | "void"
            | "await"
            | "try"
            | "catch"
            | "finally"
            | "function"
            | "class"
            | "const"
            | "let"
            | "var"
            | "in"
            | "of"
            | "do"
    )
}

fn is_known_unqualified_fn(value: &str) -> bool {
    matches!(
        value,
        "listOf"
            | "arrayOf"
            | "getString"
            | "getBoolean"
            | "GET"
            | "Page"
            | "MangasPage"
            | "Exception"
            | "LinkedHashMap"
            | "ArrayList"
    )
}

fn lower_try_expression(node: &Node, source: &str) -> JsExpr {
    // try_expression: try { ... } catch (e) { ... } finally { ... }
    // Simplified: convert to a call that handles try/catch
    let mut try_body: Option<JsExpr> = None;
    let mut catch_body: Option<JsExpr> = None;

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i as u32) {
            match child.kind() {
                "block" if try_body.is_none() => {
                    try_body = Some(lower_branch_as_expr(&child, source));
                }
                "block" if catch_body.is_none() => {
                    catch_body = Some(lower_branch_as_expr(&child, source));
                }
                _ => {}
            }
        }
    }

    // For now, emit as a raw template - proper try/catch is complex
    let try_text = try_body.map(|e| emit_expr(&e)).unwrap_or_default();
    let catch_text = catch_body.map(|e| emit_expr(&e)).unwrap_or_default();
    
    JsExpr::Raw(format!(
        "(() => {{ try {{ return {}; }} catch (_) {{ return {}; }} }})()",
        try_text, catch_text
    ))
}

fn lower_if_statement(node: &Node, source: &str) -> JsStmt {
    let condition = node
        .child_by_field_name("condition")
        .map(|cond| lower_expr(&cond, source))
        .unwrap_or_else(|| JsExpr::Unsupported(node_text(node, source)));

    let mut branches: Vec<Node> = Vec::new();
    for i in 0..node.named_child_count() {
        if let Some(named) = node.named_child(i as u32) {
            if let Some(cond) = node.child_by_field_name("condition") {
                if cond.start_byte() == named.start_byte() && cond.end_byte() == named.end_byte() {
                    continue;
                }
            }
            branches.push(named);
        }
    }

    let then_body = branches
        .first()
        .map(|branch| lower_branch_as_body(branch, source))
        .unwrap_or_default();
    let else_body = branches.get(1).map(|branch| lower_branch_as_body(branch, source));

    JsStmt::If {
        condition,
        then_body,
        else_body,
    }
}

fn lower_var_decl(node: &Node, source: &str) -> JsStmt {
    let (name, init) = lower_property_parts(node, source);
    JsStmt::VarDecl { name, init }
}

fn lower_property_parts(node: &Node, source: &str) -> (String, Option<JsExpr>) {
    let name = find_first_identifier_text(node, source)
        .map(|n| sanitize_identifier(&n))
        .unwrap_or_else(|| String::from("value"));

    let mut init = None;
    let mut seen_equals = false;

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i as u32) {
            if child.is_extra() {
                continue;
            }

            match child.kind() {
                "=" => seen_equals = true,
                _ if seen_equals => {
                    init = Some(lower_expr(&child, source));
                    break;
                }
                _ => {}
            }
        }
    }

    (name, init)
}

fn lower_return_statement(node: &Node, source: &str) -> JsStmt {
    if node.kind() == "jump_expression" {
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i as u32) {
                if child.kind() == "return_expression" {
                    return lower_return_statement(&child, source);
                }
            }
        }
    }

    if node.kind() == "return_expression" {
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i as u32) {
                if child.is_extra() || child.kind() == "return" {
                    continue;
                }
                return JsStmt::Return(Some(lower_expr(&child, source)));
            }
        }
        return JsStmt::Return(None);
    }

    let text = node_text(node, source);
    let trimmed = text.trim();

    if !trimmed.starts_with("return") {
        return JsStmt::Unsupported(trimmed.to_string());
    }

    let expr_text = trimmed.strip_prefix("return").unwrap_or("").trim();
    if expr_text.is_empty() {
        JsStmt::Return(None)
    } else {
        JsStmt::Return(Some(JsExpr::Unsupported(expr_text.to_string())))
    }
}

fn lower_expr(node: &Node, source: &str) -> JsExpr {
    match node.kind() {
        "identifier" | "simple_identifier" => JsExpr::Ident(sanitize_identifier(&node_text(node, source))),
        "integer_literal" | "number_literal" | "real_literal" => JsExpr::Number(node_text(node, source)),
        "string_literal" => JsExpr::String(node_text(node, source)),
        "this_expression" => JsExpr::Ident("this".to_string()),
        "super_expression" => JsExpr::Raw("super".to_string()),
        "throw_expression" => lower_throw_expression(node, source),
        "string_template" => lower_string_template(node, source),
        "binary_expression" => lower_binary_expression(node, source),
        "call_expression" => lower_call_expression(node, source),
        "navigation_expression" => lower_navigation_expression(node, source),
        "postfix_expression" => lower_postfix_expression(node, source),
        "if_expression" => lower_if_expression(node, source),
        "when_expression" => lower_when_expression(node, source),
        "lambda_expression" | "lambda_literal" => lower_lambda_expression(node, source),
        "try_expression" => lower_try_expression(node, source),
        "parenthesized_expression" => {
            if let Some(expr_child) = first_named_child(node) {
                lower_expr(&expr_child, source)
            } else {
                JsExpr::Unsupported(node_text(node, source))
            }
        }
        _ => lower_expr_text(&node_text(node, source)),
    }
}

fn lower_throw_expression(node: &Node, source: &str) -> JsExpr {
    for i in 0..node.child_count() {
        if let Some(child) = node.child(i as u32) {
            if child.is_extra() || child.kind() == "throw" {
                continue;
            }

            let expr = lower_expr(&child, source);
            return JsExpr::Raw(format!("throw {}", emit_expr(&expr)));
        }
    }

    JsExpr::Raw(format!("throw {}", node_text(node, source).trim_start_matches("throw").trim()))
}

fn lower_expr_text(text: &str) -> JsExpr {
    let trimmed = text.trim();

    if trimmed.is_empty() {
        return JsExpr::Raw(String::new());
    }

    if let Some(prefix) = trimmed.strip_suffix("!!.values.toList()") {
        return JsExpr::Raw(format!("Array.from({}.values())", prefix));
    }

    if let Some(prefix) = trimmed.strip_suffix(".values.toList()") {
        return JsExpr::Raw(format!("Array.from({}.values())", prefix));
    }

    if let Some(prefix) = trimmed.strip_suffix("!!") {
        return lower_expr_text(prefix);
    }

    if trimmed == "this" {
        return JsExpr::Ident("this".to_string());
    }

    if trimmed == "null" || trimmed == "true" || trimmed == "false" {
        return JsExpr::Raw(trimmed.to_string());
    }

    if trimmed.chars().all(|ch| ch.is_ascii_alphanumeric() || ch == '_' || ch == '$') {
        return JsExpr::Ident(sanitize_identifier(trimmed));
    }

    JsExpr::Raw(trimmed.to_string())
}

fn lower_if_expression(node: &Node, source: &str) -> JsExpr {
    let condition = node
        .child_by_field_name("condition")
        .map(|cond| lower_expr(&cond, source));

    let mut branches: Vec<Node> = Vec::new();
    for i in 0..node.named_child_count() {
        if let Some(named) = node.named_child(i as u32) {
            if let Some(cond) = node.child_by_field_name("condition") {
                if cond.start_byte() == named.start_byte() && cond.end_byte() == named.end_byte() {
                    continue;
                }
            }
            branches.push(named);
        }
    }

    let then_branch = branches.first();
    let then_expr = then_branch.map(|branch| lower_branch_as_expr(branch, source));
    let else_expr = branches.get(1).map(|branch| lower_branch_as_expr(branch, source));

    if let (Some(condition), Some(branch), None) = (condition.clone(), then_branch, else_expr.clone()) {
        let then_body = lower_branch_as_body(branch, source);
        let mut out = String::new();
        out.push_str("if (");
        out.push_str(&emit_expr(&condition));
        out.push_str(") {\n");
        for stmt in then_body {
            emit_stmt(&stmt, 1, &mut out);
        }
        out.push_str("}");
        return JsExpr::Raw(out);
    }

    match (condition, then_expr, else_expr) {
        (Some(condition), Some(then_expr), Some(else_expr)) => JsExpr::Conditional {
            condition: Box::new(condition),
            then_expr: Box::new(qualify_lambda_receiver_calls(then_expr)),
            else_expr: Box::new(qualify_lambda_receiver_calls(else_expr)),
        },
        // Kotlin allows if as a statement without else; preserve it in output.
        (Some(condition), Some(then_expr), None) => JsExpr::Raw(format!(
            "if ({}) {{ {}; }}",
            emit_expr(&condition),
            emit_expr(&qualify_lambda_receiver_calls(then_expr))
        )),
        _ => JsExpr::Unsupported(node_text(node, source)),
    }
}

fn lower_when_expression(node: &Node, source: &str) -> JsExpr {
    let mut subject: Option<JsExpr> = None;
    let mut entries: Vec<Node> = Vec::new();

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i as u32) {
            match child.kind() {
                "when_subject" => {
                    subject = first_named_child(&child).map(|n| lower_expr(&n, source));
                }
                "when_entry" => entries.push(child),
                _ => {}
            }
        }
    }

    if entries.is_empty() {
        return JsExpr::Unsupported(node_text(node, source));
    }

    let mut default_expr = JsExpr::Raw("undefined".to_string());
    let mut conditions_and_values: Vec<(JsExpr, JsExpr)> = Vec::new();

    for entry in entries {
        let cond_text = when_entry_condition_text(&entry, source);
        let value = when_entry_value_expr(&entry, source);

        if cond_text.trim() == "else" || cond_text.trim().is_empty() {
            default_expr = value;
            continue;
        }

        let condition = build_when_condition_expr(cond_text.trim(), subject.as_ref());
        conditions_and_values.push((condition, value));
    }

    if conditions_and_values.is_empty() {
        return default_expr;
    }

    let mut expr = default_expr;
    for (condition, value) in conditions_and_values.into_iter().rev() {
        expr = JsExpr::Conditional {
            condition: Box::new(condition),
            then_expr: Box::new(value),
            else_expr: Box::new(expr),
        };
    }

    expr
}

fn when_entry_condition_text(node: &Node, source: &str) -> String {
    if let Some(condition) = node.child_by_field_name("condition") {
        return node_text(&condition, source);
    }

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i as u32) {
            if child.kind() == "when_condition" {
                return node_text(&child, source);
            }
        }
    }

    String::new()
}

fn when_entry_value_expr(node: &Node, source: &str) -> JsExpr {
    if let Some(consequence) = node.child_by_field_name("consequence") {
        return lower_branch_as_expr(&consequence, source);
    }

    for i in (0..node.named_child_count()).rev() {
        if let Some(named) = node.named_child(i as u32) {
            if named.kind() == "when_condition" || named.kind() == "line_comment" || named.kind() == "block_comment" {
                continue;
            }
            return lower_branch_as_expr(&named, source);
        }
    }

    JsExpr::Raw("undefined".to_string())
}

fn build_when_condition_expr(raw_condition: &str, subject: Option<&JsExpr>) -> JsExpr {
    let parts = raw_condition
        .split(',')
        .map(|part| part.trim())
        .filter(|part| !part.is_empty())
        .collect::<Vec<_>>();

    let mut condition_exprs: Vec<JsExpr> = Vec::new();
    for part in parts {
        let part_expr = parse_simple_expr_from_text(part);
        if let Some(subject) = subject {
            condition_exprs.push(JsExpr::Binary {
                left: Box::new(subject.clone()),
                op: "===".to_string(),
                right: Box::new(part_expr),
            });
        } else {
            condition_exprs.push(part_expr);
        }
    }

    if condition_exprs.is_empty() {
        return JsExpr::Raw("false".to_string());
    }

    let mut combined = condition_exprs.remove(0);
    for expr in condition_exprs {
        combined = JsExpr::Binary {
            left: Box::new(combined),
            op: "||".to_string(),
            right: Box::new(expr),
        };
    }

    combined
}

fn parse_simple_expr_from_text(raw: &str) -> JsExpr {
    let trimmed = raw.trim();
    if trimmed.starts_with('"') || trimmed.starts_with('\'') || trimmed.starts_with('`') {
        JsExpr::String(trimmed.to_string())
    } else if trimmed.parse::<f64>().is_ok() {
        JsExpr::Number(trimmed.to_string())
    } else if trimmed == "true" || trimmed == "false" || trimmed == "null" {
        JsExpr::Raw(trimmed.to_string())
    } else if trimmed.chars().all(|ch| ch.is_ascii_alphanumeric() || ch == '_' || ch == '$') {
        JsExpr::Ident(sanitize_identifier(trimmed))
    } else {
        JsExpr::Raw(trimmed.to_string())
    }
}

fn lower_branch_as_expr(node: &Node, source: &str) -> JsExpr {
    if node.kind() == "block" {
        let mut exprs: Vec<JsExpr> = Vec::new();
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i as u32) {
                if child.is_extra() || child.kind() == "{" || child.kind() == "}" {
                    continue;
                }
                match lower_statement(&child, source) {
                    JsStmt::Expr(expr) => exprs.push(expr),
                    JsStmt::Return(Some(expr)) => exprs.push(expr),
                    JsStmt::Unsupported(text) if text.is_empty() => {}
                    stmt => {
                        let mut rendered = String::new();
                        emit_stmt(&stmt, 0, &mut rendered);
                        let trimmed = rendered.trim();
                        if !trimmed.is_empty() {
                            exprs.push(JsExpr::Raw(trimmed.trim_end_matches(';').to_string()));
                        }
                    }
                }
            }
        }
        if exprs.is_empty() {
            JsExpr::Raw("undefined".to_string())
        } else if exprs.len() == 1 {
            exprs.into_iter().next().unwrap()
        } else {
            let mut body = String::from("(() => { ");
            for (index, expr) in exprs.iter().enumerate() {
                if index + 1 == exprs.len() {
                    body.push_str("return ");
                    body.push_str(&emit_expr(expr));
                    body.push_str("; ");
                } else {
                    body.push_str(&emit_expr(expr));
                    body.push_str("; ");
                }
            }
            body.push_str("})()");
            JsExpr::Raw(body)
        }
    } else {
        lower_expr(node, source)
    }
}

fn lower_branch_as_body(node: &Node, source: &str) -> Vec<JsStmt> {
    if node.kind() == "block" {
        return lower_block(node, source);
    }

    vec![lower_statement(node, source)]
}

fn lower_call_expression(node: &Node, source: &str) -> JsExpr {
    let mut callee = JsExpr::Unsupported(String::from("unknownCall"));
    let mut args = Vec::new();

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i as u32) {
            match child.kind() {
                "identifier"
                | "simple_identifier"
                | "navigation_expression"
                | "postfix_expression"
                | "this_expression"
                | "super_expression" => {
                    callee = lower_expr(&child, source);
                }
                "annotated_lambda" => {
                    // Trailing lambda directly in call_expression: method { ... }
                    if let Some(lambda_child) = first_named_child(&child) {
                        if lambda_child.kind() == "lambda_literal" {
                            args.push(lower_lambda_expression(&lambda_child, source));
                        } else if lambda_child.kind() == "lambda_expression" {
                            args.push(lower_lambda_expression(&lambda_child, source));
                        }
                    }
                }
                "call_suffix" | "value_arguments" => {
                    args.extend(lower_call_args(&child, source));
                    
                    // Also extract lambda expressions that might be in the call_suffix
                    for j in 0..child.child_count() {
                        if let Some(arg_child) = child.child(j as u32) {
                            if arg_child.kind() == "lambda_expression" {
                                args.push(lower_lambda_expression(&arg_child, source));
                            }
                        }
                    }
                }
                "lambda_expression" => {
                    // Trailing lambda: method { ... }
                    args.push(lower_lambda_expression(&child, source));
                }
                _ => {}
            }
        }
    }

    if let JsExpr::Ident(name) = &callee {
        if name == "LinkedHashMap" {
            return JsExpr::Raw("new Map()".to_string());
        }

        if name == "listOf" || name == "arrayOf" {
            return JsExpr::Raw(format!(
                "[{}]",
                args.iter().map(emit_expr).collect::<Vec<_>>().join(", ")
            ));
        }

        if name == "ArrayList" {
            return match args.as_slice() {
                [] => JsExpr::Raw("[]".to_string()),
                [single] => JsExpr::Raw(format!("[...{}]", emit_expr(single))),
                _ => JsExpr::Raw(format!("[{}]", args.iter().map(emit_expr).collect::<Vec<_>>().join(", "))),
            };
        }
    }

    if let JsExpr::Member { object, member } = &callee {
        if member == "addAll" && args.len() == 1 {
            return JsExpr::Raw(format!("{}.push(...{})", emit_expr(object), emit_expr(&args[0])));
        }
    }

    JsExpr::Call {
        callee: Box::new(callee),
        args,
    }
}

fn lower_call_args(node: &Node, source: &str) -> Vec<JsExpr> {
    let mut args = Vec::new();

    for i in 0..node.child_count() {
        if let Some(child) = node.child(i as u32) {
            match child.kind() {
                "value_argument" => {
                    let mut parsed = None;
                    for j in 0..child.child_count() {
                        if let Some(arg_child) = child.child(j as u32) {
                            if arg_child.is_extra() || arg_child.kind() == "," || arg_child.kind() == "=" {
                                continue;
                            }
                            parsed = Some(lower_expr(&arg_child, source));
                        }
                    }
                    if let Some(expr) = parsed {
                        args.push(expr);
                    }
                }
                // Handle lambda expressions as arguments
                "lambda_expression" => {
                    args.push(lower_lambda_expression(&child, source));
                }
                // Handle function references like screen::addPreference
                "callable_reference" => {
                    let text = node_text(&child, source);
                    if let Some((owner, method)) = text.split_once("::") {
                        args.push(JsExpr::Member {
                            object: Box::new(lower_expr_text(owner.trim())),
                            member: sanitize_identifier(method.trim()),
                        });
                    } else if let Some(method) = text.split("::").last() {
                        args.push(JsExpr::Ident(sanitize_identifier(method.trim())));
                    } else {
                        args.push(JsExpr::Raw(text));
                    }
                }
                _ => {}
            }
        }
    }

    args
}


fn lower_binary_expression(node: &Node, source: &str) -> JsExpr {
    let left = node
        .child_by_field_name("left")
        .map(|child| lower_expr(&child, source));
    let right = node
        .child_by_field_name("right")
        .map(|child| lower_expr(&child, source));

    if let (Some(left), Some(right)) = (left, right) {
        let raw_op = extract_binary_operator(node, source).unwrap_or_else(|| String::from("+"));
        let op = normalize_binary_operator(&raw_op);

        if op == "??" {
            if let JsExpr::Raw(raw_right) = &right {
                let trimmed_right = raw_right.trim_start();
                if trimmed_right.starts_with("throw ") {
                    return JsExpr::Raw(format!(
                        "({} ?? (() => {{ {}; }})())",
                        emit_expr(&left),
                        trimmed_right
                    ));
                }
            }
        }

        JsExpr::Binary {
            left: Box::new(left),
            op,
            right: Box::new(right),
        }
    } else {
        JsExpr::Unsupported(node_text(node, source))
    }
}

fn extract_binary_operator(node: &Node, source: &str) -> Option<String> {
    let left = node.child_by_field_name("left")?;
    let right = node.child_by_field_name("right")?;
    let start = left.end_byte();
    let end = right.start_byte();
    let raw = source.get(start..end)?.trim();

    if raw.is_empty() {
        None
    } else {
        Some(raw.to_string())
    }
}

fn normalize_binary_operator(raw_op: &str) -> String {
    match raw_op.trim() {
        "?:" => "??".to_string(),
        "and" => "&&".to_string(),
        "or" => "||".to_string(),
        "xor" => "^".to_string(),
        "shl" => "<<".to_string(),
        "shr" => ">>".to_string(),
        "ushr" => ">>>".to_string(),
        other => other.to_string(),
    }
}

fn first_named_child<'a>(node: &Node<'a>) -> Option<Node<'a>> {
    for i in 0..node.named_child_count() {
        if let Some(child) = node.named_child(i as u32) {
            return Some(child);
        }
    }
    None
}

fn find_first_identifier_text(node: &Node, source: &str) -> Option<String> {
    for i in 0..node.child_count() {
        if let Some(child) = node.child(i as u32) {
            match child.kind() {
                "identifier" | "simple_identifier" => return Some(node_text(&child, source)),
                _ => {
                    if let Some(found) = find_first_identifier_text(&child, source) {
                        return Some(found);
                    }
                }
            }
        }
    }

    None
}

fn node_text(node: &Node, source: &str) -> String {
    let start = node.start_byte();
    let end = node.end_byte();
    if end <= source.len() {
        source[start..end].to_string()
    } else {
        String::new()
    }
}

fn emit_stmt(stmt: &JsStmt, indent: usize, out: &mut String) {
    let pad = "  ".repeat(indent);
    match stmt {
        JsStmt::Import(line) => {
            out.push_str(&pad);
            out.push_str(line.trim());
            if !line.trim_end().ends_with(';') {
                out.push(';');
            }
            out.push('\n');
        }
        JsStmt::Function(function) => emit_function(function, indent, false, out),
        JsStmt::Class(class_decl) => emit_class(class_decl, indent, out),
        JsStmt::VarDecl { name, init } => {
            out.push_str(&pad);
            out.push_str("let ");
            out.push_str(name);
            if let Some(expr) = init {
                out.push_str(" = ");
                out.push_str(&emit_expr(expr));
            }
            out.push_str(";\n");
        }
        JsStmt::If {
            condition,
            then_body,
            else_body,
        } => {
            out.push_str(&pad);
            out.push_str("if (");
            out.push_str(&emit_expr(condition));
            out.push_str(") {\n");
            for stmt in then_body {
                emit_stmt(stmt, indent + 1, out);
            }
            out.push_str(&pad);
            out.push('}');

            if let Some(else_body) = else_body {
                out.push_str(" else {\n");
                for stmt in else_body {
                    emit_stmt(stmt, indent + 1, out);
                }
                out.push_str(&pad);
                out.push_str("}\n");
            } else {
                out.push('\n');
            }
        }
        JsStmt::Expr(expr) => {
            out.push_str(&pad);
            out.push_str(&emit_expr(expr));
            out.push_str(";\n");
        }
        JsStmt::Return(expr) => {
            out.push_str(&pad);
            out.push_str("return");
            if let Some(expr) = expr {
                out.push(' ');
                out.push_str(&emit_expr(expr));
            }
            out.push_str(";\n");
        }
        JsStmt::Unsupported(text) => {
            if text.trim().is_empty() {
                return;
            }
            out.push_str(&pad);
            out.push_str("/* unsupported Kotlin node: ");
            out.push_str(&sanitize_comment(text));
            out.push_str(" */\n");
        }
    }
}

fn emit_function(function: &JsFunction, indent: usize, as_method: bool, out: &mut String) {
    let pad = "  ".repeat(indent);
    out.push_str(&pad);

    if function.is_async {
        out.push_str("async ");
    }

    if !as_method {
        out.push_str("function ");
    }

    out.push_str(&function.name);
    out.push('(');
    out.push_str(&function.params.join(", "));
    out.push_str(") {\n");

    for stmt in &function.body {
        emit_stmt(stmt, indent + 1, out);
    }

    out.push_str(&pad);
    out.push_str("}\n");
}

fn emit_class(class_decl: &JsClass, indent: usize, out: &mut String) {
    let pad = "  ".repeat(indent);
    out.push_str(&pad);
    out.push_str("class ");
    out.push_str(&class_decl.name);
    out.push_str(" {\n");

    let mut method_counts: HashMap<String, usize> = HashMap::new();
    let static_field_names = class_decl
        .members
        .iter()
        .filter_map(|member| match member {
            JsClassMember::Field {
                name,
                is_static: true,
                ..
            } => Some(name.clone()),
            _ => None,
        })
        .collect::<Vec<_>>();
    let instance_field_names = class_decl
        .members
        .iter()
        .filter_map(|member| match member {
            JsClassMember::Field {
                name,
                is_static: false,
                ..
            } => Some(name.clone()),
            _ => None,
        })
        .collect::<Vec<_>>();

    for member in &class_decl.members {
        if let JsClassMember::Method(method) = member {
            *method_counts.entry(method.name.clone()).or_insert(0) += 1;
        }
    }

    let mut emitted_dispatchers: HashSet<String> = HashSet::new();

    for member in &class_decl.members {
        match member {
            JsClassMember::Field {
                name,
                init,
                is_static,
            } => {
                out.push_str(&"  ".repeat(indent + 1));
                if *is_static {
                    out.push_str("static ");
                }
                out.push_str(name);
                if let Some(expr) = init {
                    out.push_str(" = ");
                    out.push_str(&emit_expr(expr));
                }
                out.push_str(";\n");
            }
            JsClassMember::Method(method) => {
                let overload_count = method_counts.get(&method.name).copied().unwrap_or(0);
                if overload_count > 1 {
                    if emitted_dispatchers.insert(method.name.clone()) {
                        let overloads = class_decl
                            .members
                            .iter()
                            .filter_map(|candidate| match candidate {
                                JsClassMember::Method(candidate_method)
                                    if candidate_method.name == method.name =>
                                {
                                    Some(candidate_method)
                                }
                                _ => None,
                            })
                            .collect::<Vec<_>>();

                        emit_overloaded_method_dispatcher(
                            &method.name,
                            &overloads,
                            indent + 1,
                            &instance_field_names,
                            &static_field_names,
                            out,
                        );
                    }
                } else {
                    emit_method_with_context(
                        method,
                        indent + 1,
                        &instance_field_names,
                        &static_field_names,
                        out,
                    );
                }
            }
        }
    }

    out.push_str(&pad);
    out.push_str("}\n");
}

fn emit_overloaded_method_dispatcher(
    method_name: &str,
    overloads: &[&JsFunction],
    indent: usize,
    instance_field_names: &[String],
    static_field_names: &[String],
    out: &mut String,
) {
    if overloads.is_empty() {
        return;
    }

    let pad = "  ".repeat(indent);
    let mut ordered = overloads.to_vec();
    ordered.sort_by_key(|function| function.params.len());

    let max_arity = ordered.iter().map(|function| function.params.len()).max().unwrap_or(0);
    let dispatch_params = (0..max_arity)
        .map(|idx| format!("arg{}", idx))
        .collect::<Vec<_>>();
    let is_async = ordered.iter().any(|function| function.is_async);

    out.push_str(&pad);
    if is_async {
        out.push_str("async ");
    }
    out.push_str(method_name);
    out.push('(');
    out.push_str(&dispatch_params.join(", "));
    out.push_str(") {\n");

    emit_method_context_prelude(indent + 1, instance_field_names, static_field_names, out);

    // Allow bare self-calls from lowered Kotlin to resolve to the class method.
    out.push_str(&"  ".repeat(indent + 1));
    out.push_str(&format!(
        "let {} = (...args) => this.{}(...args);\n",
        method_name, method_name
    ));

    let mut arities = ordered
        .iter()
        .map(|function| function.params.len())
        .collect::<Vec<_>>();
    arities.sort_unstable();
    arities.dedup();

    for arity in arities {
        let variants = ordered
            .iter()
            .copied()
            .filter(|function| function.params.len() == arity)
            .collect::<Vec<_>>();

        if variants.len() == 1 {
            emit_overload_branch(variants[0], arity, None, &dispatch_params, indent + 1, out);
            continue;
        }

        let mut emitted_type_branch = false;
        for variant in &variants {
            let guard = build_overload_type_guard(variant, &dispatch_params);
            if let Some(guard_expr) = guard {
                emit_overload_branch(
                    variant,
                    arity,
                    Some(guard_expr.as_str()),
                    &dispatch_params,
                    indent + 1,
                    out,
                );
                emitted_type_branch = true;
            }
        }

        // Fallback for this arity if no type guard matches.
        // This keeps behavior deterministic even when names do not convey types.
        if emitted_type_branch {
            emit_overload_branch(variants[0], arity, None, &dispatch_params, indent + 1, out);
        } else {
            emit_overload_branch(variants[0], arity, None, &dispatch_params, indent + 1, out);
        }
    }

    out.push_str(&"  ".repeat(indent + 1));
    out.push_str(&format!(
        "throw new Error(\"No matching overload for {} with \" + arguments.length + \" args\");\n",
        method_name
    ));

    out.push_str(&pad);
    out.push_str("}\n");
}

fn emit_method_with_context(
    function: &JsFunction,
    indent: usize,
    instance_field_names: &[String],
    static_field_names: &[String],
    out: &mut String,
) {
    let pad = "  ".repeat(indent);
    out.push_str(&pad);

    if function.is_async {
        out.push_str("async ");
    }

    out.push_str(&function.name);
    out.push('(');
    out.push_str(&function.params.join(", "));
    out.push_str(") {\n");

    emit_method_context_prelude(indent + 1, instance_field_names, static_field_names, out);

    for stmt in &function.body {
        emit_stmt(stmt, indent + 1, out);
    }

    out.push_str(&pad);
    out.push_str("}\n");
}

fn emit_method_context_prelude(
    indent: usize,
    instance_field_names: &[String],
    static_field_names: &[String],
    out: &mut String,
) {
    if !instance_field_names.is_empty() {
        out.push_str(&"  ".repeat(indent));
        out.push_str("let { ");
        out.push_str(&instance_field_names.join(", "));
        out.push_str(" } = this;\n");
    }

    if !static_field_names.is_empty() {
        out.push_str(&"  ".repeat(indent));
        out.push_str("let { ");
        out.push_str(&static_field_names.join(", "));
        out.push_str(" } = this.constructor;\n");
    }
}

fn emit_overload_branch(
    overload: &JsFunction,
    arity: usize,
    extra_guard: Option<&str>,
    dispatch_params: &[String],
    indent: usize,
    out: &mut String,
) {
    let pad = "  ".repeat(indent);
    out.push_str(&pad);

    if let Some(guard) = extra_guard {
        out.push_str(&format!("if (arguments.length === {} && ({})) {{\n", arity, guard));
    } else {
        out.push_str(&format!("if (arguments.length === {}) {{\n", arity));
    }

    for (idx, param_name) in overload.params.iter().enumerate() {
        out.push_str(&"  ".repeat(indent + 1));
        out.push_str("let ");
        out.push_str(param_name);
        out.push_str(" = ");
        out.push_str(&dispatch_params[idx]);
        out.push_str(";\n");
    }

    for stmt in &overload.body {
        emit_stmt(stmt, indent + 1, out);
    }

    out.push_str(&pad);
    out.push_str("}\n");
}

fn build_overload_type_guard(overload: &JsFunction, dispatch_params: &[String]) -> Option<String> {
    let mut guards = Vec::new();

    for (idx, param_name) in overload.params.iter().enumerate() {
        if let Some(arg_name) = dispatch_params.get(idx) {
            if let Some(guard) = infer_param_guard(param_name, arg_name) {
                guards.push(guard);
            }
        }
    }

    if guards.is_empty() {
        None
    } else {
        Some(guards.join(" && "))
    }
}

fn infer_param_guard(param_name: &str, arg_name: &str) -> Option<String> {
    let lower = param_name.to_ascii_lowercase();

    if lower.contains("items") || lower.contains("list") || lower.contains("array") {
        return Some(format!("Array.isArray({})", arg_name));
    }

    if lower.starts_with("is") || lower.starts_with("has") || lower.contains("enabled") {
        return Some(format!("typeof {} === \"boolean\"", arg_name));
    }

    if lower.contains("page")
        || lower.contains("index")
        || lower.contains("count")
        || lower.contains("limit")
        || lower.contains("offset")
        || lower.contains("size")
        || lower.ends_with("num")
        || lower.contains("number")
    {
        return Some(format!("typeof {} === \"number\"", arg_name));
    }

    if lower.contains("hash")
        || lower.contains("query")
        || lower.contains("path")
        || lower.contains("url")
        || lower.contains("title")
        || lower.contains("name")
        || lower.contains("lang")
        || lower.ends_with("key")
    {
        return Some(format!("typeof {} === \"string\"", arg_name));
    }

    if lower.contains("map")
        || lower.contains("dict")
        || lower.contains("set")
        || lower.contains("manga")
        || lower.contains("chapter")
        || lower.contains("response")
        || lower.contains("screen")
        || lower.contains("filter")
        || lower.contains("pref")
        || lower.contains("builder")
    {
        return Some(format!(
            "{} != null && typeof {} === \"object\" && !Array.isArray({})",
            arg_name, arg_name, arg_name
        ));
    }

    None
}

fn emit_expr(expr: &JsExpr) -> String {
    match expr {
        JsExpr::Ident(name) => name.clone(),
        JsExpr::Number(value) => value.clone(),
        JsExpr::String(value) => emit_kotlin_string_literal(value),
        JsExpr::Raw(value) => {
            let trimmed = value.trim();
            let candidate = trimmed
                .strip_prefix('(')
                .and_then(|text| text.strip_suffix(')'))
                .unwrap_or(trimmed);

            if let Some(index) = candidate.find("?? throw ") {
                let left = candidate[..index].trim();
                let right = candidate[index + "?? throw ".len()..].trim();
                return format!("({} ?? (() => {{ throw {}; }})())", left, right);
            }

            if let Some(index) = candidate.find("?: throw ") {
                let left = candidate[..index].trim();
                let right = candidate[index + "?: throw ".len()..].trim();
                return format!("({} ?? (() => {{ throw {}; }})())", left, right);
            }

            value.clone()
        }
        JsExpr::Call { callee, args } => {
            let args = args.iter().map(emit_expr).collect::<Vec<_>>().join(", ");
            format!("{}({})", emit_expr(callee), args)
        }
        JsExpr::Member { object, member } => {
            format!("{}.{}", emit_expr(object), member)
        }
        JsExpr::Index { object, index } => {
            format!("{}[{}]", emit_expr(object), emit_expr(index))
        }
        JsExpr::Binary { left, op, right } => {
            if op == "??" {
                let right_text = emit_expr(right);
                if right_text.trim_start().starts_with("throw ") {
                    return format!(
                        "({} ?? (() => {{ {}; }})())",
                        emit_expr(left),
                        right_text.trim()
                    );
                }
            }

            format!("({} {} {})", emit_expr(left), op, emit_expr(right))
        }
        JsExpr::Conditional {
            condition,
            then_expr,
            else_expr,
        } => format!(
            "({} ? {} : {})",
            emit_expr(condition),
            emit_expr(then_expr),
            emit_expr(else_expr)
        ),
        JsExpr::Lambda { params, body } => {
            format!("({}) => {}", params.join(", "), emit_expr(body))
        }
        JsExpr::Template { parts, expressions } => {
            // Build a template literal: `part1 ${expr1} part2 ${expr2} ...`
            let mut result = String::from("`");
            for (i, part) in parts.iter().enumerate() {
                result.push_str(part);
                if i < expressions.len() {
                    result.push_str("${");
                    result.push_str(&emit_expr(&expressions[i]));
                    result.push('}');
                }
            }
            result.push('`');
            result
        }
        JsExpr::Unsupported(text) => {
            format!("undefined /* unsupported expr: {} */", sanitize_comment(text))
        }
    }
}

fn emit_kotlin_string_literal(value: &str) -> String {
    let trimmed = value.trim();
    if !(trimmed.starts_with('"') && trimmed.ends_with('"') && trimmed.len() >= 2) {
        return value.to_string();
    }

    let inner = &trimmed[1..trimmed.len() - 1];
    if !inner.contains('$') {
        return value.to_string();
    }

    let chars = inner.chars().collect::<Vec<_>>();
    let mut out = String::from("`");
    let mut i = 0;

    while i < chars.len() {
        let ch = chars[i];

        if ch == '\\' {
            if i + 1 < chars.len() {
                let next = chars[i + 1];
                if next == '$' {
                    out.push('$');
                    i += 2;
                    continue;
                }
                if next == '`' {
                    out.push_str("\\`");
                    i += 2;
                    continue;
                }
                out.push('\\');
                out.push(next);
                i += 2;
                continue;
            }
            out.push('\\');
            i += 1;
            continue;
        }

        if ch == '`' {
            out.push_str("\\`");
            i += 1;
            continue;
        }

        if ch == '$' {
            if i + 1 < chars.len() && chars[i + 1] == '{' {
                let mut j = i + 2;
                let mut depth = 1usize;
                let mut expr = String::new();

                while j < chars.len() {
                    let current = chars[j];
                    if current == '{' {
                        depth += 1;
                        expr.push(current);
                    } else if current == '}' {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                        expr.push(current);
                    } else {
                        expr.push(current);
                    }
                    j += 1;
                }

                out.push_str("${");
                out.push_str(expr.trim());
                out.push('}');
                i = if j < chars.len() { j + 1 } else { j };
                continue;
            }

            if i + 1 < chars.len() && is_js_ident_start(chars[i + 1]) {
                let mut j = i + 2;
                while j < chars.len() && is_js_ident_part(chars[j]) {
                    j += 1;
                }

                let ident = chars[i + 1..j].iter().collect::<String>();
                out.push_str("${");
                out.push_str(&ident);
                out.push('}');
                i = j;
                continue;
            }
        }

        out.push(ch);
        i += 1;
    }

    out.push('`');
    out
}

fn is_js_ident_start(ch: char) -> bool {
    ch == '_' || ch == '$' || ch.is_ascii_alphabetic()
}

fn is_js_ident_part(ch: char) -> bool {
    is_js_ident_start(ch) || ch.is_ascii_digit()
}

fn rewrite_import(raw_import: &str) -> String {
    let raw = raw_import.trim();
    let path = raw.strip_prefix("import").unwrap_or(raw).trim();

    if path.is_empty() {
        return String::from("/* empty import */");
    }

    let (path, alias) = match path.split_once(" as ") {
        Some((import_path, import_alias)) => (import_path.trim(), Some(import_alias.trim())),
        None => (path, None),
    };

    if let Some(module_path) = path.strip_suffix(".*") {
        let namespace = sanitize_identifier(module_path);
        return format!("import * as {} from \"{}\";", namespace, module_path);
    }

    let mut parts: Vec<&str> = path.split('.').collect();
    if parts.len() < 2 {
        return format!("import \"{}\";", path);
    }

    let symbol_name = parts.pop().unwrap_or_default();
    let module_path = parts.join(".");

    match alias {
        Some(import_alias) => format!(
            "import {{ {} as {} }} from \"{}\";",
            symbol_name,
            sanitize_identifier(import_alias),
            module_path
        ),
        None => format!("import {{ {} }} from \"{}\";", symbol_name, module_path),
    }
}

fn sanitize_identifier(value: &str) -> String {
    let mut out = value
        .chars()
        .map(|ch| {
            if ch.is_ascii_alphanumeric() || ch == '_' || ch == '$' {
                ch
            } else {
                '_'
            }
        })
        .collect::<String>();

    if out
        .chars()
        .next()
        .map(|ch| ch.is_ascii_digit())
        .unwrap_or(true)
    {
        out.insert(0, '_');
    }

    out
}

fn sanitize_comment(text: &str) -> String {
    text.replace("*/", "* /").replace('\n', " ").trim().to_string()
}

#[cfg(test)]
mod tests {
    use std::ops::Deref;

    use super::*;
    use crate::{parse_kotlin_code};
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
    fn transpiles_simple_function() {
        let source = r#"
fun hello(name: String) {
    println(name)
}
"#;
        let tree = parse_kotlin_code(source);
        let js = KotlinTranspiler::transpile(source, &tree);

        assert!(js.contains("function hello"));
        assert!(js.contains("println(name)"));
    }

    #[test]
    fn transpiles_simple_class() {
        let source = r#"
class User {
    val id = 1
}
"#;
        let tree = parse_kotlin_code(source);
        let js = KotlinTranspiler::transpile(source, &tree);

        assert!(js.contains("class User"));
        assert!(js.contains("id = 1"));
    }

    #[test]
    fn transpiles_if_expression_initializer() {
        let source = r#"
fun score(v: Int): Int {
    val out = if (v > 10) 1 else 0
    return out
}
"#;
        let tree = parse_kotlin_code(source);
        let js = KotlinTranspiler::transpile(source, &tree);

        assert!(js.contains("(v > 10) ? 1 : 0") || js.contains("v > 10 ? 1 : 0"));
    }

    #[test]
    fn transpiles_when_expression_initializer() {
        let source = r#"
fun label(v: Int): String {
    val name = when (v) {
        1 -> "one"
        2, 3 -> "few"
        else -> "many"
    }
    return name
}
"#;
        let tree = parse_kotlin_code(source);
        let js = KotlinTranspiler::transpile(source, &tree);

        assert!(js.contains("v === 1"));
        assert!(js.contains("v === 2") || js.contains("v === 3"));
    }

    #[test]
    fn transpiles_elvis_operator_as_nullish_coalescing() {
        let source = r#"
fun pick(v: String?): String {
    val out = v ?: "fallback"
    return out
}
"#;
        let tree = parse_kotlin_code(source);
        let js = KotlinTranspiler::transpile(source, &tree);

        assert!(js.contains("??"));
        assert!(!js.contains("?:"));
    }

    #[test]
    fn transpiles_kotlin_string_interpolation_to_js_template_literal() {
        let source = r#"
fun getUrl(baseUrl: String, manga: Manga): String {
    return "$baseUrl/title${manga.url}"
}
"#;
        let tree = parse_kotlin_code(source);
        let js = KotlinTranspiler::transpile(source, &tree);

        assert!(js.contains("`${baseUrl}/title${manga.url}`"));
        assert!(!js.contains("\"$baseUrl/title${manga.url}\""));
    }

    #[test]
    fn full_file_test() {
        let file = std::fs::read_to_string("temp/full_test.kt").expect("Failed to read test file");
        let tree = parse_kotlin_code(&file);
        let js = KotlinTranspiler::transpile(&file, &tree);
        println!("Transpiled JS:\n{}", js);

        let formatted_js = try_format_with_oxc(&js);
        if let Some(formatted) = formatted_js {
            println!("Formatted JS:\n{}", formatted);
        } else {
            println!("Failed to format JS with Oxc.");
        }

        // log the JS to a file for manual inspection if needed
        std::fs::write("temp/full_test_output.js", &js).expect("Failed to write output JS file");
    }
}
