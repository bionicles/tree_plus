//! Markdown and plain-text extraction (legacy `parse_md`, `parse_txt`).

use std::sync::LazyLock;

use regex::Regex;

const MARKDOWN_LANGUAGES: &[&str] = &["md", "markdown", "mdx", "mdc"];

static TASK_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(-\s*\[ *[xX]?\])\s*(.*)").unwrap());
static URL_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r#"\s*\(<a href=".*">.+</a>\)|<a href=".*">.+</a>"#).unwrap());
static LINK_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\(.*?\)").unwrap());

/// Extract headers and tasks from Markdown (legacy `parse_md`).
pub fn extract_md(content: &str) -> Vec<String> {
    let mut headers_and_tasks: Vec<String> = Vec::new();
    // (task_text, include_flag) of checked ancestors
    let mut checked_ancestors: Vec<(String, bool)> = Vec::new();
    let mut code_block_stack: Vec<String> = Vec::new();

    for line in content.split('\n') {
        let line = line.strip_suffix('\r').unwrap_or(line);
        let stripped_line = line.trim();
        if let Some(after_fence) = stripped_line.strip_prefix("```") {
            if !code_block_stack.is_empty() {
                if stripped_line == "```" {
                    code_block_stack.pop();
                } else {
                    code_block_stack.push(after_fence.trim().to_string());
                }
            } else {
                code_block_stack.push(after_fence.trim().to_string());
            }
            continue;
        }

        let in_opaque_block = code_block_stack
            .last()
            .map(|lang| !MARKDOWN_LANGUAGES.contains(&lang.as_str()))
            .unwrap_or(false);
        if in_opaque_block {
            continue;
        }

        if line.starts_with('#') {
            let line = line.trim_start();
            let clean = URL_RE.replace_all(line, "");
            let clean = LINK_RE.replace_all(&clean, "");
            let clean = clean.trim();
            if clean.trim_start_matches('#').trim().is_empty() {
                continue;
            }
            headers_and_tasks.push(clean.to_string());
        } else if TASK_RE.is_match(line.trim_start())
            && TASK_RE
                .captures(line.trim_start())
                .map(|c| c.get(0).map(|m| m.start() == 0).unwrap_or(false))
                .unwrap_or(false)
        {
            let lstripped = line.trim_start();
            let indent_level = line.len() - lstripped.len();
            if let Some(caps) = TASK_RE.captures(lstripped) {
                let task_text = caps.get(2).map(|m| m.as_str()).unwrap_or("");
                let is_checked = line.contains("[x]") || line.contains("[X]");
                let task = format!(
                    "{}{}{}",
                    " ".repeat(indent_level),
                    if is_checked { "- [x] " } else { "- [ ] " },
                    task_text
                );
                checked_ancestors.retain(|a| a.0.len() < task.len());
                if is_checked {
                    checked_ancestors.push((task, false));
                } else {
                    for a in checked_ancestors.iter_mut() {
                        a.1 = true;
                    }
                    headers_and_tasks.extend(checked_ancestors.iter().map(|a| a.0.clone()));
                    headers_and_tasks.push(task);
                }
            }
        }
    }
    headers_and_tasks
}

static CHECKBOX_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"-\s*\[\s*([^Xx])?\s*\]\s*(.+)").unwrap());

/// Extract unchecked checkboxes from plain text (legacy `parse_txt`).
pub fn extract_txt(content: &str) -> Vec<String> {
    content
        .split('\n')
        .filter_map(|line| {
            CHECKBOX_RE
                .captures(line)
                .and_then(|caps| caps.get(2))
                .map(|m| format!("- [ ] {}", m.as_str().trim()))
        })
        .collect()
}

/// Extract RST headings (legacy `parse_rst`): a content line followed by an
/// underline of `=` (heading, `# `) or `-` (subheading, `- `).
pub fn extract_rst(content: &str) -> Vec<String> {
    let lines: Vec<&str> = content.split('\n').collect();
    let mut components = Vec::new();
    let mut i = 0;
    while i + 1 < lines.len() {
        let underline = lines[i + 1].trim_end_matches('\r');
        let is_sub = !underline.is_empty() && underline.chars().all(|c| c == '-');
        let is_head = !underline.is_empty() && underline.chars().all(|c| c == '=');
        if is_sub || is_head {
            let text = lines[i].trim_end_matches('\r');
            let prefix = if is_head { "# " } else { "- " };
            components.push(format!("{prefix}{text}"));
            i += 2;
        } else {
            i += 1;
        }
    }
    components
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn headers_and_code_blocks() {
        let content = "# Title\n```rust\n# not a header\n```\n## Sub\n";
        assert_eq!(extract_md(content), vec!["# Title", "## Sub"]);
    }

    #[test]
    fn markdown_code_blocks_stay_transparent() {
        let content = "# A\n```md\n# Inner\n```\n";
        assert_eq!(extract_md(content), vec!["# A", "# Inner"]);
    }

    #[test]
    fn checked_ancestors_logic() {
        let content = "- [x] done parent\n  - [ ] open child\n";
        assert_eq!(
            extract_md(content),
            vec!["- [x] done parent", "  - [ ] open child"]
        );
    }

    #[test]
    fn checked_without_open_children_hidden() {
        let content = "- [x] done alone\n- [ ] open\n";
        assert_eq!(extract_md(content), vec!["- [ ] open"]);
    }

    #[test]
    fn header_links_removed() {
        assert_eq!(
            extract_md("# Hello [link](https://x.y) world\n"),
            vec!["# Hello [link] world"]
        );
    }

    #[test]
    fn txt_checkboxes() {
        assert_eq!(
            extract_txt("- [ ] do it\n- [x] done\ntext\n"),
            vec!["- [ ] do it"]
        );
    }

    #[test]
    fn rst_headings() {
        assert_eq!(
            extract_rst("Title\n=====\n\nSub\n---\n"),
            vec!["# Title", "- Sub"]
        );
    }
}
