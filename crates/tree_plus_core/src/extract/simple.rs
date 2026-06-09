//! Line-oriented extractors: .env, requirements.txt, Makefile/Justfile,
//! and the Angular special cases from the legacy TypeScript dispatch.

use std::sync::LazyLock;

use regex::Regex;

static ENV_KEY_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"([A-Z|_]*)=").unwrap());

/// Python-`str.splitlines()`-alike for `\n` / `\r\n` / `\r` (no trailing empty).
pub fn splitlines(content: &str) -> Vec<&str> {
    let mut out = Vec::new();
    let mut start = 0;
    let bytes = content.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        match bytes[i] {
            b'\n' => {
                out.push(&content[start..i]);
                i += 1;
                start = i;
            }
            b'\r' => {
                out.push(&content[start..i]);
                i += 1;
                if i < bytes.len() && bytes[i] == b'\n' {
                    i += 1;
                }
                start = i;
            }
            _ => i += 1,
        }
    }
    if start < bytes.len() {
        out.push(&content[start..]);
    }
    out
}

/// Legacy `parse_dot_env`: first `KEY=` match per non-comment line.
pub fn dot_env(content: &str) -> Vec<String> {
    splitlines(content)
        .into_iter()
        .filter(|line| !line.starts_with('#'))
        .filter_map(|line| {
            ENV_KEY_RE
                .captures(line)
                .and_then(|c| c.get(1))
                .map(|m| m.as_str().to_string())
        })
        .collect()
}

/// Legacy `parse_requirements_txt`: every line not starting with `#`.
pub fn requirements_txt(content: &str) -> Vec<String> {
    splitlines(content)
        .into_iter()
        .filter(|line| !line.starts_with('#'))
        .map(|line| line.to_string())
        .collect()
}

/// Legacy `parse_makefile`: targets, includes, defines, .PHONY lines.
pub fn makefile(content: &str) -> Vec<String> {
    content
        .split('\n')
        .filter(|line| {
            let stripped = line.trim();
            !stripped.is_empty()
                && !line.starts_with('\t')
                && !stripped.starts_with('#')
                && (line.starts_with(".PHONY")
                    || line.contains(':')
                    || line.starts_with("include")
                    || line.starts_with("define"))
        })
        .map(|line| line.trim().trim_end_matches(':').to_string())
        .collect()
}

static DESCRIBE_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(\t*| *)describe\('(.*)'").unwrap());
static IT_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r#"(\t*| *)it\(('|")(.*)('|")"#).unwrap());

/// Legacy `parse_angular_spec`: describe/it lines.
pub fn angular_spec(content: &str) -> Vec<String> {
    let mut components = Vec::new();
    for line in content.split('\n') {
        if let Some(caps) = DESCRIBE_RE.captures(line) {
            components.push(format!(
                "{}describe '{}'",
                caps.get(1).map_or("", |m| m.as_str()),
                caps.get(2).map_or("", |m| m.as_str())
            ));
        } else if let Some(caps) = IT_RE.captures(line) {
            let statement = caps
                .get(3)
                .map_or("", |m| m.as_str())
                .replace('\\', "")
                .replace('"', "'");
            components.push(format!(
                "{}it {}",
                caps.get(1).map_or("", |m| m.as_str()),
                statement
            ));
        }
    }
    components
}

static ROUTES_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(?s)(const routes: Routes = \[\n.*?\];)").unwrap());

/// Legacy `parse_angular_routes`.
pub fn angular_routes(content: &str) -> Vec<String> {
    ROUTES_RE
        .captures(content)
        .and_then(|c| c.get(1))
        .map(|m| vec![m.as_str().to_string()])
        .unwrap_or_default()
}

static NG_MODULE_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"(@NgModule\(\{\n\s*declarations: \[((\n\s*)[a-zA-Z]*,?)*)\n").unwrap()
});

/// Legacy `parse_angular_app_module`.
pub fn angular_app_module(content: &str) -> Vec<String> {
    NG_MODULE_RE
        .captures(content)
        .and_then(|c| c.get(1))
        .map(|m| vec![m.as_str().to_string()])
        .unwrap_or_default()
}

static ENV_TS_KEY_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"(\w+):").unwrap());

/// Legacy `parse_environment_ts`: keys of `export const environment = {...}`.
pub fn environment_ts(content: &str) -> Vec<String> {
    let mut parsing = false;
    let mut keepers: Vec<String> = Vec::new();
    for line in content.split('\n') {
        let stripped = line.trim();
        if stripped.starts_with("export const environment = {") {
            parsing = true;
            continue;
        }
        if stripped.starts_with("};") {
            break;
        }
        if stripped.starts_with("//") || stripped.starts_with("/*") || stripped.starts_with('*') {
            continue;
        }
        if parsing {
            if let Some(caps) = ENV_TS_KEY_RE.captures(line) {
                keepers.push(format!("   {}", caps.get(1).map_or("", |m| m.as_str())));
            }
        }
    }
    if keepers.is_empty() {
        keepers
    } else {
        let mut out = vec!["environment:".to_string()];
        out.extend(keepers);
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn env_keys() {
        assert_eq!(
            dot_env("API_KEY=abc\n# comment SECRET=no\nDB_URL=postgres\n"),
            vec!["API_KEY", "DB_URL"]
        );
    }

    #[test]
    fn env_lowercase_key_yields_empty_match() {
        // legacy quirk: ([A-Z|_]*) matches empty before '='
        assert_eq!(dot_env("lower=1\n"), vec![""]);
    }

    #[test]
    fn makefile_targets() {
        let content =
            "SHELL := /bin/bash\n.PHONY: test\ntest:\n\tpytest\n# comment:\ninclude foo.mk\n";
        assert_eq!(
            makefile(content),
            vec![
                "SHELL := /bin/bash",
                ".PHONY: test",
                "test",
                "include foo.mk"
            ]
        );
    }

    #[test]
    fn requirements_keeps_blank_lines() {
        assert_eq!(requirements_txt("a==1\n# c\n\nb\n"), vec!["a==1", "", "b"]);
    }

    #[test]
    fn spec_describe_it() {
        let content = "describe('AppComponent', () => {\n  it('should work', () => {});\n});\n";
        assert_eq!(
            angular_spec(content),
            vec!["describe 'AppComponent'", "  it should work"]
        );
    }
}
