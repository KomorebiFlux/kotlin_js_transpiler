use tree_sitter::{Parser};

pub mod transpiler;
pub use transpiler::KotlinTranspiler;

pub fn parse_kotlin_code(code: &str) -> tree_sitter::Tree {
    // Load the Kotlin language
    let language = &tree_sitter_kotlin_ng::LANGUAGE.into();

    // Create a parser and set the language
    let mut parser = Parser::new();
    parser.set_language(language).expect("Error loading Kotlin grammar");

    // Parse the code and return the syntax tree
    parser.parse(code, None).expect("Error parsing code")
}

/// Transpile Kotlin code to JavaScript
pub fn transpile_kotlin_to_js(code: &str) -> String {
    let tree = parse_kotlin_code(code);
    KotlinTranspiler::transpile(code, &tree)
}

const FULL_TEST_SOURCE_CODE: &str = include_str!("../temp/full_test.kt");

const SIMPLE_TEST_SOURCE_CODE: &str = r#"fun main() {
    println("Hello, World!")
}"#;

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn parse_full_test() {
        // Initialize the parser
        let tree = parse_kotlin_code(FULL_TEST_SOURCE_CODE);

        // Access the root node
        let root_node = tree.root_node();

        // Inspect the tree structure (S-expression format)
        println!("AST: {}", root_node.to_sexp());
    }

    #[test]
    fn parse_simple_test() {
        // Initialize the parser
        let tree = parse_kotlin_code(SIMPLE_TEST_SOURCE_CODE);

        // Access the root node
        let root_node = tree.root_node();

        // Inspect the tree structure (S-expression format)
        println!("AST: {}", root_node.to_sexp());
    }

    #[test]
    fn test_transpile_simple() {
        let js_code = transpile_kotlin_to_js(SIMPLE_TEST_SOURCE_CODE);
        assert!(!js_code.is_empty());
        println!("Transpiled JS:\n{}", js_code);
    }

    #[test]
    fn test_transpile_full() {
        let js_code = transpile_kotlin_to_js(FULL_TEST_SOURCE_CODE);
        println!("Transpiled JS:\n{}", js_code);
        assert!(!js_code.is_empty());
        assert!(js_code.contains("class") || js_code.contains("function"));
        assert!(!js_code.contains("Oxc full-program parse failed; applying best-effort function-level formatting."));
    }

    #[test]
    fn test_apply_with_lambda() {
        let code = r#"fun test() {
    val url = builder.apply {
        addQueryParameter("key", "val")
    }.build()
}"#;
        let js_code = transpile_kotlin_to_js(code);
        println!("Transpiled JS:\n{}", js_code);
    }
}