//! Deterministic tree rendering byte-compatible with the legacy renderer
//! (rich.tree.Tree printed via a no-color Console at a fixed width).
//!
//! Reimplements the relevant parts of rich:
//! - tree guides: `"    "`, `"│   "`, `"├── "`, `"└── "`;
//! - word wrapping (`rich._wrap.divide_line` with fold=True) measured in
//!   terminal cells (emoji are 2 cells wide);
//! - trailing-space removal (legacy `remove_trailing_space`).

use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};

use crate::model::{commafy, Category, TreePlus};

pub const DEFAULT_WIDTH: usize = 80;

const FILE_CHAR: &str = "📄";
const FOLDER_CHAR: &str = "📁";
const ROOT_CHAR: &str = "🌵";
const GLOB_CHAR: &str = "🌀";

fn plural(n: u64) -> &'static str {
    if n == 1 {
        ""
    } else {
        "s"
    }
}

/// Build the display label for a node (legacy `_into_rich_tree`).
pub fn node_label(node: &TreePlus) -> String {
    match node.category {
        Category::File => {
            let tokens = node.n_tokens();
            let lines = node.n_lines();
            format!(
                "{FILE_CHAR} {} ({} token{}, {} line{})",
                node.name,
                commafy(tokens),
                plural(tokens),
                commafy(lines),
                plural(lines),
            )
        }
        Category::Folder => {
            let folders = node.n_folders();
            let files = node.n_files();
            format!(
                "{FOLDER_CHAR} {} ({} folder{}, {} file{})",
                node.name,
                commafy(folders),
                plural(folders),
                commafy(files),
                plural(files),
            )
        }
        Category::Root => {
            let mut label = format!("{ROOT_CHAR} {}", node.name);
            if node.n_tokens() > 0 && node.n_lines() > 0 {
                let folders = node.n_folders();
                let files = node.n_files();
                label.push_str(&format!(
                    " ({} folder{}, {} file{})",
                    commafy(folders),
                    plural(folders),
                    commafy(files),
                    plural(files),
                ));
            }
            label
        }
        Category::Glob => {
            let n = node.subtrees.len() as u64;
            format!(
                "{GLOB_CHAR} {} ({} match{})",
                node.name,
                commafy(n),
                if n == 1 { "" } else { "es" },
            )
        }
        Category::Component => node.name.clone(),
    }
}

/// Iterate "words" like rich's `re.compile(r"\s*\S+\s*")`.
fn words(text: &str) -> Vec<(usize, String)> {
    let chars: Vec<char> = text.chars().collect();
    let mut out = Vec::new();
    let mut pos = 0;
    while pos < chars.len() {
        let start = pos;
        let mut i = pos;
        while i < chars.len() && chars[i].is_whitespace() {
            i += 1;
        }
        if i >= chars.len() {
            break; // trailing whitespace w/o a word: no match
        }
        while i < chars.len() && !chars[i].is_whitespace() {
            i += 1;
        }
        while i < chars.len() && chars[i].is_whitespace() {
            i += 1;
        }
        out.push((start, chars[start..i].iter().collect()));
        pos = i;
    }
    out
}

fn cell_len(s: &str) -> usize {
    UnicodeWidthStr::width(s)
}

/// Split text into chunks of at most `width` cells (rich `chop_cells`).
fn chop_cells(text: &str, width: usize) -> Vec<String> {
    let mut lines: Vec<String> = vec![String::new()];
    let mut total_width = 0;
    for ch in text.chars() {
        let w = UnicodeWidthChar::width(ch).unwrap_or(0);
        if total_width + w > width {
            lines.push(ch.to_string());
            total_width = w;
        } else {
            lines.last_mut().unwrap().push(ch);
            total_width += w;
        }
    }
    lines
}

/// Port of `rich._wrap.divide_line` (fold=True). Returns char offsets.
fn divide_line(text: &str, width: usize) -> Vec<usize> {
    let mut break_positions: Vec<usize> = Vec::new();
    let mut cell_offset = 0usize;
    for (start, word) in words(text) {
        let word_length = cell_len(word.trim_end());
        let remaining_space = width.saturating_sub(cell_offset);
        if remaining_space >= word_length {
            cell_offset += cell_len(&word);
        } else if word_length > width {
            // fold the word across multiple lines
            let folded = chop_cells(&word, width);
            let mut start = start;
            let n = folded.len();
            for (i, line) in folded.iter().enumerate() {
                if start > 0 {
                    break_positions.push(start);
                }
                if i + 1 == n {
                    cell_offset = cell_len(line);
                } else {
                    start += line.chars().count();
                }
            }
        } else if cell_offset > 0 && start > 0 {
            break_positions.push(start);
            cell_offset = cell_len(&word);
        }
    }
    break_positions
}

/// Wrap one logical line into physical lines at `width` cells.
fn wrap_line(line: &str, width: usize) -> Vec<String> {
    let width = width.max(1);
    let breaks = divide_line(line, width);
    if breaks.is_empty() {
        return vec![line.to_string()];
    }
    let chars: Vec<char> = line.chars().collect();
    let mut out = Vec::new();
    let mut prev = 0;
    for &b in &breaks {
        out.push(chars[prev..b].iter().collect());
        prev = b;
    }
    out.push(chars[prev..].iter().collect());
    out
}

/// Expand tabs to 8-cell stops like rich's `Text.expand_tabs`.
fn expand_tabs(line: &str, tab_size: usize) -> String {
    if !line.contains('\t') {
        return line.to_string();
    }
    let mut out = String::with_capacity(line.len());
    let mut col = 0;
    for ch in line.chars() {
        if ch == '\t' {
            let pad = tab_size - (col % tab_size);
            out.extend(std::iter::repeat_n(' ', pad));
            col += pad;
        } else {
            out.push(ch);
            col += UnicodeWidthChar::width(ch).unwrap_or(0);
        }
    }
    out
}

/// Wrap a (possibly multiline) label into physical lines.
fn wrap_label(label: &str, width: usize) -> Vec<String> {
    let mut out = Vec::new();
    for logical in label.split('\n') {
        out.extend(wrap_line(&expand_tabs(logical, 8), width));
    }
    out
}

struct Renderer {
    width: usize,
    out: String,
}

impl Renderer {
    fn emit(&mut self, prefix_first: &str, prefix_rest: &str, label: &str) {
        let avail = self.width.saturating_sub(cell_len(prefix_first)).max(1);
        for (i, line) in wrap_label(label, avail).into_iter().enumerate() {
            let prefix = if i == 0 { prefix_first } else { prefix_rest };
            let mut rendered = format!("{prefix}{line}");
            while rendered.ends_with(' ') {
                rendered.pop();
            }
            self.out.push_str(&rendered);
            self.out.push('\n');
        }
    }

    fn render_node(&mut self, node: &TreePlus, ancestors: &str, is_root: bool, is_last: bool) {
        let (first, rest) = if is_root {
            (String::new(), String::new())
        } else {
            let fork = if is_last { "└── " } else { "├── " };
            let cont = if is_last { "    " } else { "│   " };
            (format!("{ancestors}{fork}"), format!("{ancestors}{cont}"))
        };
        self.emit(&first, &rest, &node_label(node));

        let child_ancestors = if is_root {
            String::new()
        } else if is_last {
            format!("{ancestors}    ")
        } else {
            format!("{ancestors}│   ")
        };

        if node.category == Category::File {
            let n = node.components.len();
            for (i, component) in node.components.iter().enumerate() {
                let last = i + 1 == n;
                let fork = if last { "└── " } else { "├── " };
                let cont = if last { "    " } else { "│   " };
                self.emit(
                    &format!("{child_ancestors}{fork}"),
                    &format!("{child_ancestors}{cont}"),
                    component,
                );
            }
        } else {
            let n = node.subtrees.len();
            for (i, sub) in node.subtrees.iter().enumerate() {
                self.render_node(sub, &child_ancestors, false, i + 1 == n);
            }
        }
    }
}

/// Render a tree to a string identical to legacy `TreePlus.into_str()`.
pub fn render_to_string(root: &TreePlus, width: usize) -> String {
    let mut r = Renderer {
        width,
        out: String::new(),
    };
    r.render_node(root, "", true, true);
    r.out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{Category, TreePlus};

    #[test]
    fn single_file_render() {
        let mut file = TreePlus::new(Category::File, "file.py");
        file.token_count = 19;
        file.line_count = 3;
        file.components.push("def hello_world()".to_string());
        let s = render_to_string(&file, DEFAULT_WIDTH);
        assert_eq!(
            s,
            "📄 file.py (19 tokens, 3 lines)\n└── def hello_world()\n"
        );
    }

    #[test]
    fn wrap_matches_rich_probe() {
        // mirrors a probe of the legacy renderer
        let mut root = TreePlus::new(Category::Component, "root");
        root.category = Category::Folder; // unused; emit manually below
        let long = "function julia_is_awesome(prob::DiffEqBase.AbstractDAEProblem{uType, duType, tType, isinplace};";
        let mut r = Renderer {
            width: 80,
            out: String::new(),
        };
        r.emit("├── ", "│   ", long);
        assert_eq!(
            r.out,
            "├── function julia_is_awesome(prob::DiffEqBase.AbstractDAEProblem{uType, duType,\n│   tType, isinplace};\n"
        );
    }

    #[test]
    fn fold_long_word() {
        let mut r = Renderer {
            width: 80,
            out: String::new(),
        };
        let label = format!("word {} tail", "x".repeat(100));
        r.emit("├── ", "│   ", &label);
        let expected = format!(
            "├── word\n│   {}\n│   {} tail\n",
            "x".repeat(76),
            "x".repeat(24)
        );
        assert_eq!(r.out, expected);
    }

    #[test]
    fn multiline_label_continuation() {
        let mut folder = TreePlus::new(Category::Folder, "f");
        let mut file = TreePlus::new(Category::File, "a.py");
        file.components
            .push("    @staticmethod\n    def m()".to_string());
        file.components.push("class X".to_string());
        folder.subtrees.push(file);
        let s = render_to_string(&folder, DEFAULT_WIDTH);
        let expected = "\
📁 f (1 folder, 1 file)
└── 📄 a.py (0 tokens, 0 lines)
    ├──     @staticmethod
    │       def m()
    └── class X
";
        assert_eq!(s, expected);
    }
}
