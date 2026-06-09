//! Debug helper: dump a tree-sitter AST outline.
//! Usage: cargo run -p tree_plus_core --example dump_ast -- <file> [start_line] [end_line]

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let path = &args[0];
    let lo: usize = args.get(1).and_then(|s| s.parse().ok()).unwrap_or(0);
    let hi: usize = args
        .get(2)
        .and_then(|s| s.parse().ok())
        .unwrap_or(usize::MAX);
    let content = std::fs::read_to_string(path).unwrap();
    let ext = format!(
        ".{}",
        std::path::Path::new(path)
            .extension()
            .unwrap()
            .to_string_lossy()
    );
    let language: tree_sitter::Language = match ext.as_str() {
        ".cpp" | ".cc" | ".cu" | ".hpp" => tree_sitter_cpp::LANGUAGE.into(),
        ".c" | ".h" => tree_sitter_c::LANGUAGE.into(),
        ".py" => tree_sitter_python::LANGUAGE.into(),
        ".rs" => tree_sitter_rust::LANGUAGE.into(),
        ".js" | ".ts" => tree_sitter_typescript::LANGUAGE_TYPESCRIPT.into(),
        ".go" => tree_sitter_go::LANGUAGE.into(),
        ".java" => tree_sitter_java::LANGUAGE.into(),
        ".swift" => tree_sitter_swift::LANGUAGE.into(),
        other => panic!("no grammar for {other}"),
    };
    let mut parser = tree_sitter::Parser::new();
    parser.set_language(&language).unwrap();
    let tree = parser.parse(&content, None).unwrap();
    let mut stack = vec![(tree.root_node(), 0usize)];
    while let Some((node, depth)) = stack.pop() {
        let row = node.start_position().row + 1;
        if row >= lo && row <= hi {
            let text: String = content[node.byte_range()]
                .chars()
                .take(60)
                .collect::<String>()
                .replace('\n', "\\n");
            println!(
                "{}{} [{}..{}] {:?}",
                "  ".repeat(depth),
                node.kind(),
                row,
                node.end_position().row + 1,
                text
            );
        }
        let mut cursor = node.walk();
        let children: Vec<_> = node.named_children(&mut cursor).collect();
        for child in children.into_iter().rev() {
            stack.push((child, depth + 1));
        }
    }
}
