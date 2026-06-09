//! C and C++ component extraction (tree-sitter).
//!
//! Emits source-text slices in legacy `parse_c` style:
//! - function definitions (signature through initializer list, no body);
//! - struct/class/enum definitions with their fields and enumerators;
//! - access specifiers, typedef struct + `} name;` closers, #define macros,
//!   template declarations.
//!
//! Intentional differences from the legacy regex (documented in
//! docs/rust-port-differences.md): control-flow noise (e.g. `while(...)`
//! lines), string contents, and the TensorFlow flag special case are not
//! emitted.

use std::sync::LazyLock;

use regex::Regex;
use tree_sitter::Node;

use super::rust::strip_c_comments;
use super::{parse, ExtractResult};

/// Legacy "function" form: `^ *(modifier )?(return_type )(name)\(args\)` with
/// at most two words before the name and no `)` inside the args.
static FUNCTION_FORM_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"^ *(?:[\w:]+ )?[\w:*&]+(?:\s?<[^>]*>\s?)? [\w*&\[\]]+\([^)]*\)$").unwrap()
});

/// Legacy "method" form prefix (before the parameter list): indented,
/// optional virtual/static, optional single-word return type, `~?[*\w]+`.
static METHOD_PREFIX_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^\s+(?:virtual |static )?(?:\w+ )?~?[*\w]+$").unwrap());

/// Extract C/C++ components. `ext` picks the grammar (.c/.h use C unless C++
/// constructs are likely; .cpp/.cc/.cu/.cuh/.hpp use C++).
pub fn extract(content: &str, ext: &str) -> ExtractResult {
    let use_cpp =
        matches!(ext, ".cpp" | ".cc" | ".cu" | ".cuh" | ".hpp") || looks_like_cpp(content);
    let language: tree_sitter::Language = if use_cpp {
        tree_sitter_cpp::LANGUAGE.into()
    } else {
        tree_sitter_c::LANGUAGE.into()
    };
    let tree = parse(content, &language)?;
    let mut extractor = CExtractor {
        content,
        components: Vec::new(),
        suppress_plain_fields: 0,
    };
    extractor.visit(tree.root_node());
    Ok(extractor.components)
}

/// Cheap heuristic so C++ headers using .h still parse with the C++ grammar.
fn looks_like_cpp(content: &str) -> bool {
    content.contains("class ")
        || content.contains("namespace ")
        || content.contains("template")
        || content.contains("::")
}

struct CExtractor<'a> {
    content: &'a str,
    components: Vec<String>,
    /// Legacy quirk: a `template<...> class/struct` line reset the regex
    /// context, so plain data fields inside template records were skipped.
    suppress_plain_fields: usize,
}

fn line_start(content: &str, byte: usize) -> usize {
    content[..byte].rfind('\n').map(|i| i + 1).unwrap_or(0)
}

impl<'a> CExtractor<'a> {
    fn slice_clean(&self, from: usize, to: usize) -> String {
        // legacy parse_c ran on comment-stripped content
        strip_c_comments(&self.content[from..to])
            .trim_end()
            .trim_start_matches('\n')
            .to_string()
    }

    fn visit(&mut self, node: Node<'a>) {
        match node.kind() {
            "function_definition" => {
                if let Some(body) = node.child_by_field_name("body") {
                    self.emit_function_definition(node, body);
                    self.visit(body);
                } else if let Some(declarator) = find_function_declarator(node) {
                    // `= default;` / `= delete;` members have no body node
                    let start = line_start(self.content, node.start_byte());
                    if let Some(component) =
                        self.method_form_component(start, node, declarator.end_byte())
                    {
                        self.components.push(component);
                    }
                }
            }
            "ERROR" => {
                self.salvage_error_region(node);
            }
            "struct_specifier" | "class_specifier" | "union_specifier" => {
                self.emit_record(node, None);
            }
            "enum_specifier" => {
                self.emit_enum(node);
            }
            "type_definition" => {
                self.emit_type_definition(node);
            }
            "declaration" => {
                self.emit_declaration(node);
            }
            "field_declaration" => {
                self.emit_field(node);
            }
            "access_specifier" => {
                // includes the trailing ':' in the slice
                let start = line_start(self.content, node.start_byte());
                let mut end = node.end_byte();
                if self.content[end..].starts_with(':') {
                    end += 1;
                }
                self.components.push(self.slice_clean(start, end));
            }
            "preproc_def" | "preproc_function_def" => {
                // legacy: `#define` + same-line invocation (greedy to the
                // last `)` on the first line, excluding `\` continuations)
                static DEFINE_RE: LazyLock<Regex> =
                    LazyLock::new(|| Regex::new(r"^#define(\s\w+( ?\w* ?\(.*\))?)?").unwrap());
                let first_line = self.content[node.byte_range()]
                    .split('\n')
                    .next()
                    .unwrap_or("");
                if let Some(m) = DEFINE_RE.find(first_line) {
                    self.components.push(m.as_str().trim_end().to_string());
                }
            }
            "template_declaration" => {
                self.emit_template(node);
            }
            "expression_statement" => {
                // legacy pybind noise: ` *\w+.def("...` captured to first `;`
                self.maybe_emit_pybind_def(node);
                // legacy method-pattern noise: indented `asm(...)` statements
                if let Some(child) = node.named_child(0) {
                    if child.kind().contains("asm") {
                        let start = line_start(self.content, node.start_byte());
                        let indent = &self.content[start..node.start_byte()];
                        if !indent.is_empty() && indent.chars().all(char::is_whitespace) {
                            let text = self.slice_clean(start, node.end_byte());
                            self.components
                                .push(text.trim_end_matches(';').trim_end().to_string());
                        }
                    }
                }
                let mut cursor = node.walk();
                for child in node.named_children(&mut cursor) {
                    self.visit(child);
                }
            }
            "while_statement" | "for_statement" | "if_statement" | "switch_statement" => {
                self.maybe_emit_control_noise(node);
                let mut cursor = node.walk();
                for child in node.named_children(&mut cursor) {
                    self.visit(child);
                }
            }
            _ => {
                let mut cursor = node.walk();
                for child in node.named_children(&mut cursor) {
                    self.visit(child);
                }
            }
        }
    }

    /// struct/class/union with a body: label + fields (+ optional closer).
    /// `prefix_start` overrides the slice start (e.g. `typedef `/`static `).
    fn emit_record(&mut self, node: Node<'a>, prefix_start: Option<usize>) {
        let Some(body) = node.child_by_field_name("body") else {
            return; // bare type references are not components
        };
        let start = prefix_start.unwrap_or_else(|| line_start(self.content, node.start_byte()));
        let label = self.slice_clean(start, body.start_byte());
        if !label.is_empty() {
            self.components.push(label);
        }
        self.visit(body);
    }

    fn emit_enum(&mut self, node: Node<'a>) {
        let Some(body) = node.child_by_field_name("body") else {
            return;
        };
        let start = line_start(self.content, node.start_byte());
        let label = self.slice_clean(start, body.start_byte());
        if !label.is_empty() {
            self.components.push(label);
        }
        let mut cursor = body.walk();
        for child in body.named_children(&mut cursor) {
            if child.kind() == "enumerator" {
                let estart = line_start(self.content, child.start_byte());
                let mut eend = child.end_byte();
                if self.content[eend..].starts_with(',') {
                    eend += 1;
                }
                let text = strip_c_comments(&self.content[estart..eend])
                    .trim_end()
                    .trim_start_matches('\n')
                    .replace('\t', "  ");
                self.components.push(text);
            }
        }
    }

    /// `typedef struct {...} Name;` -> "typedef struct", fields, "} Name;".
    fn emit_type_definition(&mut self, node: Node<'a>) {
        let Some(type_node) = node.child_by_field_name("type") else {
            return;
        };
        match type_node.kind() {
            "struct_specifier" | "union_specifier" | "class_specifier" => {
                if type_node.child_by_field_name("body").is_some() {
                    self.emit_record(type_node, Some(node.start_byte()));
                    self.emit_closer(type_node, node);
                }
            }
            "enum_specifier" => {
                if type_node.child_by_field_name("body").is_some() {
                    self.emit_enum(type_node);
                    self.emit_closer(type_node, node);
                }
            }
            _ => {}
        }
    }

    /// `static struct config {...} config;` and similar declarations.
    fn emit_declaration(&mut self, node: Node<'a>) {
        let Some(type_node) = node.child_by_field_name("type") else {
            return;
        };
        // legacy `other_static`: `^static (struct )?TYPE name([])?(?= =)`
        if type_node.child_by_field_name("body").is_none() {
            static OTHER_STATIC_RE: LazyLock<Regex> =
                LazyLock::new(|| Regex::new(r"^static (?:struct )?\w+ \w+(?:\[\])?").unwrap());
            let text = &self.content[node.byte_range()];
            if self.content[..node.start_byte()].ends_with('\n') {
                if let Some(m) = OTHER_STATIC_RE.find(text) {
                    if text[m.end()..].starts_with(" =") {
                        self.components.push(m.as_str().to_string());
                        return;
                    }
                }
            }
        }
        match type_node.kind() {
            "struct_specifier" | "union_specifier" | "class_specifier" => {
                if type_node.child_by_field_name("body").is_some() {
                    let start = line_start(self.content, node.start_byte());
                    self.emit_record(type_node, Some(start));
                    self.emit_closer(type_node, node);
                }
            }
            "enum_specifier" => {
                if type_node.child_by_field_name("body").is_some() {
                    self.emit_enum(type_node);
                    self.emit_closer(type_node, node);
                }
            }
            _ => {}
        }
    }

    /// The legacy `} name;` closing component for named record declarations.
    fn emit_closer(&mut self, type_node: Node<'a>, decl: Node<'a>) {
        let Some(body) = type_node.child_by_field_name("body") else {
            return;
        };
        // only when a declarator follows the body on the closing line
        if decl.end_byte() <= body.end_byte() {
            return;
        }
        let closer_text = self.content[body.end_byte() - 1..decl.end_byte()].trim_end();
        if closer_text.len() <= 2 {
            return; // "};" alone -> legacy emitted nothing
        }
        let start = line_start(self.content, body.end_byte() - 1);
        self.components
            .push(self.slice_clean(start, decl.end_byte()));
    }

    /// Function definitions must satisfy one of the two legacy forms.
    fn emit_function_definition(&mut self, node: Node<'a>, body: Node<'a>) {
        let start = line_start(self.content, node.start_byte());
        let candidate = self.slice_clean(start, body.start_byte());
        if candidate.is_empty() {
            return;
        }
        // legacy pybind11 module pattern
        if candidate.starts_with("PYBIND11_MODULE") {
            self.components.push(candidate);
            return;
        }
        // function form: the `{` follows the `)` after exactly one whitespace
        if FUNCTION_FORM_RE.is_match(&candidate)
            && single_whitespace_before(self.content, body.start_byte())
        {
            self.components.push(candidate);
            return;
        }
        // method form: indented members (with const/override/initializers)
        if let Some(component) = self.method_form_component(start, node, body.start_byte()) {
            self.components.push(component);
        }
    }

    /// Validate the legacy method form; returns the trimmed component.
    fn method_form_component(&self, start: usize, node: Node<'a>, end: usize) -> Option<String> {
        // find the parameter list of the declarator
        let declarator = find_function_declarator(node)?;
        let params = declarator.child_by_field_name("parameters")?;
        let prefix = strip_c_comments(&self.content[start..params.start_byte()]);
        if !METHOD_PREFIX_RE.is_match(&prefix) {
            return None;
        }
        // legacy args `\(.*\)` were single-line
        if self.content[params.byte_range()].contains('\n') {
            return None;
        }
        let text = self.slice_clean(start, end);
        Some(text.trim_end_matches(';').trim_end().to_string())
    }

    fn emit_field(&mut self, node: Node<'a>) {
        // legacy field patterns require an indented member starting its line
        let fstart = line_start(self.content, node.start_byte());
        let indent = &self.content[fstart..node.start_byte()];
        if indent.is_empty() || !indent.chars().all(char::is_whitespace) {
            return;
        }
        // nested records/enums inside the field (anonymous struct members)
        if let Some(type_node) = node.child_by_field_name("type") {
            if matches!(
                type_node.kind(),
                "struct_specifier" | "union_specifier" | "enum_specifier" | "class_specifier"
            ) && type_node.child_by_field_name("body").is_some()
            {
                self.visit(type_node);
                self.emit_closer(type_node, node); // e.g. `} inner;`
                return;
            }
        }
        if node_has_function_declarator(node) {
            // member function declarations follow the legacy method form;
            // trailing `= 0` / `= default` / `= delete` were never captured
            if let Some(declarator) = find_function_declarator(node) {
                if let Some(component) =
                    self.method_form_component(fstart, node, declarator.end_byte())
                {
                    self.components.push(component);
                }
            }
            return;
        }
        if self.suppress_plain_fields > 0 {
            return;
        }
        let start = fstart;
        let text = strip_c_comments(&self.content[start..node.end_byte()])
            .trim_end()
            .trim_start_matches('\n')
            .replace('\t', "  ");
        let trimmed = text.trim_end_matches(' ').to_string();
        // legacy fields had to terminate with `;` (struct) or `,` (enum)
        if !(trimmed.ends_with(';') || trimmed.ends_with(',')) {
            return;
        }
        self.components.push(trimmed);
    }

    /// Partial recovery inside ERROR regions: invalid syntax must still
    /// yield the obvious components instead of nothing.
    fn salvage_error_region(&mut self, node: Node<'a>) {
        static RECORD_HEADER_RE: LazyLock<Regex> =
            LazyLock::new(|| Regex::new(r"^[ \t]*((?:class|struct) \w+[^\n{;]*)").unwrap());
        let text = &self.content[node.byte_range()];
        if let Some(caps) = RECORD_HEADER_RE.captures(text) {
            if let Some(m) = caps.get(1) {
                self.components.push(m.as_str().trim_end().to_string());
            }
        }
        // ordered pass: salvage loose function declarators, recurse the rest
        let mut cursor = node.walk();
        for child in node.named_children(&mut cursor) {
            if child.kind() == "function_declarator" {
                let start = line_start(self.content, child.start_byte());
                let candidate = self.slice_clean(start, child.end_byte());
                if FUNCTION_FORM_RE.is_match(&candidate) {
                    let after = &self.content[child.end_byte()..];
                    let mut chars = after.chars();
                    if chars.next().is_some_and(char::is_whitespace) && chars.next() == Some('{') {
                        self.components.push(candidate);
                    }
                }
            } else {
                self.visit(child);
            }
        }
    }

    /// Legacy pybind pattern: `^ *\w+\.def\("` ... up to the first `;`.
    fn maybe_emit_pybind_def(&mut self, stmt: Node<'a>) {
        let start = line_start(self.content, stmt.start_byte());
        let region = &self.content[start..];
        static DEF_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r#"^ *\w+\.def\(""#).unwrap());
        if !DEF_RE.is_match(region) {
            return;
        }
        let Some(semi) = region.find(';') else { return };
        let text = strip_c_comments(&region[..semi])
            .trim_end()
            .trim_start_matches('\n')
            .to_string();
        self.components.push(text);
    }

    /// Legacy method-pattern noise: an indented `keyword(...)` with no space
    /// before the paren (e.g. `while((ln = listNext(&li)))`) was emitted.
    fn maybe_emit_control_noise(&mut self, node: Node<'a>) {
        let Some(cond) = node
            .child_by_field_name("condition")
            .or_else(|| node.child_by_field_name("initializer"))
        else {
            return;
        };
        let kw = node.start_byte();
        let start = line_start(self.content, kw);
        let indent = &self.content[start..kw];
        if indent.is_empty() || !indent.chars().all(|c| c == ' ' || c == '\t') {
            return;
        }
        // the legacy pattern required no whitespace between keyword and `(`
        let kw_end = kw + node.kind().split('_').next().unwrap_or("").len();
        if !self.content[kw_end..].starts_with('(') {
            return;
        }
        if self.content[cond.byte_range()].contains('\n') {
            return;
        }
        // fin: a following `\s{` (compound body) -- the common legacy case
        let after = &self.content[cond.end_byte()..];
        if !(after.starts_with(" {") || after.starts_with("\n")) {
            return;
        }
        self.components
            .push(self.slice_clean(start, cond.end_byte()).replace('\t', "  "));
    }

    fn emit_template(&mut self, node: Node<'a>) {
        // slice from `template` through the inner declaration's signature
        let start = line_start(self.content, node.start_byte());
        let mut cursor = node.walk();
        for child in node.named_children(&mut cursor) {
            match child.kind() {
                "function_definition" => {
                    if let Some(body) = child.child_by_field_name("body") {
                        let text = self.slice_clean(start, body.start_byte());
                        self.components.push(text);
                        self.visit(body);
                    }
                    return;
                }
                "declaration" => {
                    // prototype: `template <typename T> T cos(T);`
                    // (`= delete` etc. were never captured by the legacy regex)
                    let end = find_function_declarator(child)
                        .map(|d| d.end_byte())
                        .unwrap_or_else(|| child.end_byte());
                    let text = self.slice_clean(start, end);
                    self.components
                        .push(text.trim_end_matches(';').trim_end().to_string());
                    return;
                }
                "struct_specifier" | "class_specifier" => {
                    if let Some(body) = child.child_by_field_name("body") {
                        let text = self.slice_clean(start, body.start_byte());
                        self.components.push(text);
                        self.suppress_plain_fields += 1;
                        self.visit(body);
                        self.suppress_plain_fields -= 1;
                    } else {
                        let text = self.slice_clean(start, child.end_byte());
                        self.components.push(text);
                    }
                    return;
                }
                "alias_declaration" | "type_definition" => {
                    let text = self.slice_clean(start, child.end_byte());
                    self.components
                        .push(text.trim_end_matches(';').trim_end().to_string());
                    return;
                }
                _ => {}
            }
        }
    }
}

fn node_has_function_declarator(node: Node<'_>) -> bool {
    find_function_declarator(node).is_some()
}

/// Walk the declarator chain to the `function_declarator`, if any.
fn find_function_declarator(node: Node<'_>) -> Option<Node<'_>> {
    let mut current = node.child_by_field_name("declarator");
    while let Some(d) = current {
        match d.kind() {
            "function_declarator" => return Some(d),
            _ => current = d.child_by_field_name("declarator"),
        }
    }
    None
}

/// Legacy `(?=\s\{)`: exactly one whitespace char before the brace.
fn single_whitespace_before(content: &str, brace: usize) -> bool {
    let before = &content[..brace];
    before
        .chars()
        .next_back()
        .is_some_and(|c| c.is_whitespace())
        && !before.ends_with("  ")
        && !before.ends_with("\n\n")
        && !before.ends_with(" \n")
        && !before.ends_with("\n ")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn c_struct_and_functions() {
        let content = "struct Point {\n    int x;\n    int y;\n};\n\nstruct Point getOrigin() {\n    struct Point o;\n    return o;\n}\n";
        assert_eq!(
            extract(content, ".c").unwrap(),
            vec![
                "struct Point",
                "    int x;",
                "    int y;",
                "struct Point getOrigin()"
            ]
        );
    }

    #[test]
    fn typedef_struct_with_closer() {
        let content = "typedef struct {\n    char name[50];\n} Person;\n";
        assert_eq!(
            extract(content, ".c").unwrap(),
            vec!["typedef struct", "    char name[50];", "} Person;"]
        );
    }

    #[test]
    fn cpp_class_with_access() {
        let content = "class Animal {\npublic:\n    Animal(const std::string &name) : name(name) {}\n    virtual void speak() const {}\nprotected:\n    std::string name;\n};\n";
        assert_eq!(
            extract(content, ".cpp").unwrap(),
            vec![
                "class Animal",
                "public:",
                "    Animal(const std::string &name) : name(name)",
                "    virtual void speak() const",
                "protected:",
                "    std::string name;",
            ]
        );
    }

    #[test]
    fn enums_with_commas() {
        let content = "enum days {\n  SUN,\n  MON,\n  SAT\n};\n";
        assert_eq!(
            extract(content, ".c").unwrap(),
            vec!["enum days", "  SUN,", "  MON,", "  SAT"]
        );
    }
}
