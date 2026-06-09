//! Tree-sitter based extractors for the big version-1 languages:
//! Rust, Python, JavaScript/TypeScript, C and C++.
//!
//! Formatters emit source-text signature slices so output matches the legacy
//! regex-based components; golden parity tests in tests/ enforce this.

pub mod c_cpp;
pub mod go;
pub mod python;
pub mod rust;
pub mod typescript;

use std::cell::RefCell;

use tree_sitter::{Language, Node, Parser, Tree};

use super::{ExtractError, ExtractResult};

thread_local! {
    /// One parser per thread (tree-sitter parser state is mutable).
    static PARSER: RefCell<Parser> = RefCell::new(Parser::new());
}

/// Parse `content` with `language`, reusing a thread-local parser.
pub fn parse(content: &str, language: &Language) -> Result<Tree, ExtractError> {
    PARSER.with(|cell| {
        let mut parser = cell.borrow_mut();
        parser
            .set_language(language)
            .map_err(|e| ExtractError::Parse(e.to_string()))?;
        parser
            .parse(content, None)
            .ok_or_else(|| ExtractError::Parse("tree-sitter returned no tree".to_string()))
    })
}

/// UTF-8 text of a node (lossy-safe since content is valid UTF-8).
pub fn node_text<'a>(node: Node<'_>, content: &'a str) -> &'a str {
    &content[node.byte_range()]
}

/// Walk all nodes depth-first, calling `visit` on each.
pub fn walk_tree<'t, F: FnMut(Node<'t>)>(tree: &'t Tree, mut visit: F) {
    let mut cursor = tree.walk();
    let mut reached_root = false;
    while !reached_root {
        visit(cursor.node());
        if cursor.goto_first_child() {
            continue;
        }
        loop {
            if cursor.goto_next_sibling() {
                break;
            }
            if !cursor.goto_parent() {
                reached_root = true;
                break;
            }
        }
    }
}

/// Used by extractors that are still stubs.
#[allow(dead_code)]
pub fn unimplemented_language() -> ExtractResult {
    Ok(Vec::new())
}
