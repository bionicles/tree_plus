//! TODO / BUG / NOTE marker extraction (legacy `parse_markers`).

use std::sync::LazyLock;

use regex::Regex;

static MARKER_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"(?P<mark>BUG|TODO|NOTE)(?P<mention> ?\([@\w ]+\) ?)?: (?P<msg>.*)").unwrap()
});

/// Extract `MARK: message` lines from content.
pub fn extract_markers(content: &str) -> Vec<String> {
    MARKER_RE
        .captures_iter(content)
        .filter_map(|caps| {
            let mark = caps.name("mark")?.as_str();
            let msg = caps.name("msg")?.as_str();
            if msg.is_empty() {
                // legacy: falsy msg ("" only, since trailing newline is excluded)
                return None;
            }
            Some(format!("{}: {}", mark, msg.trim()))
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bug_todo_note() {
        let content = "BUG: This is a bug.\nTODO: Fix this soon.\nNOTE: Interesting observation.";
        assert_eq!(
            extract_markers(content),
            vec![
                "BUG: This is a bug.",
                "TODO: Fix this soon.",
                "NOTE: Interesting observation.",
            ]
        );
    }

    #[test]
    fn mention_is_dropped() {
        assert_eq!(
            extract_markers("TODO (@bion): hi there"),
            vec!["TODO: hi there"]
        );
    }

    #[test]
    fn empty_msg_skipped() {
        // ": " is part of the pattern, so "TODO: \n" has an empty msg
        assert!(extract_markers("TODO: \n").is_empty());
        // msg=" " (extra space) is truthy; strip() empties it -> "TODO: "
        assert_eq!(extract_markers("TODO:  \n"), vec!["TODO: "]);
        assert!(extract_markers("TODO:\n").is_empty());
    }
}
