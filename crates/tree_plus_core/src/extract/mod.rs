//! Component extraction: dispatch a file to the right extractor.
//!
//! This is the Rust analog of the legacy `parse_file` (renamed: it does not
//! parse files generically, it extracts displayable component labels).
//! Dispatch order and special cases deliberately mirror
//! `tree_plus_src/parse_file.py` because legacy behavior depends on them.

pub mod data;
pub mod markdown;
pub mod markers;
pub mod simple;
pub mod treesitter;

use std::path::Path;

/// Errors during extraction. Legacy behavior maps any extractor error to an
/// empty component list at the engine layer (including the marker pass).
#[derive(Debug, thiserror::Error)]
pub enum ExtractError {
    #[error("io: {0}")]
    Io(#[from] std::io::Error),
    #[error("parse: {0}")]
    Parse(String),
}

pub type ExtractResult = Result<Vec<String>, ExtractError>;

const BINARY_CHECK_SIZE: usize = 1024;

/// Legacy `is_binary_string`: any byte outside the text set means binary.
/// Text bytes: {7, 8, 9, 10, 12, 13, 27} | [0x20, 0xFF] - {0x7F}.
pub fn is_binary_bytes(data: &[u8]) -> bool {
    data.iter().any(|&b| {
        !(matches!(b, 7 | 8 | 9 | 10 | 12 | 13 | 27) || (0x20..=0xFF).contains(&b) && b != 0x7F)
    })
}

/// Check the first KiB of a file (legacy `is_binary`).
pub fn is_binary(path: &Path) -> bool {
    use std::io::Read;
    let Ok(mut f) = std::fs::File::open(path) else {
        return false;
    };
    let mut buf = [0u8; BINARY_CHECK_SIZE];
    let mut filled = 0;
    while filled < buf.len() {
        match f.read(&mut buf[filled..]) {
            Ok(0) => break,
            Ok(n) => filled += n,
            Err(_) => return false,
        }
    }
    is_binary_bytes(&buf[..filled])
}

/// `os.path.splitext`-compatible split: returns (stem path, lowered ".ext").
fn splitext_lower(path: &str) -> (String, String) {
    let (dir, name) = match path.rfind('/') {
        Some(i) => (&path[..=i], &path[i + 1..]),
        None => ("", path),
    };
    let trimmed = name.trim_start_matches('.');
    let n_leading = name.len() - trimmed.len();
    match trimmed.rfind('.') {
        Some(i) => {
            let split_at = n_leading + i;
            (
                format!("{dir}{}", &name[..split_at]),
                name[split_at..].to_lowercase(),
            )
        }
        None => (path.to_string(), String::new()),
    }
}

/// Read the first `n` lines like the legacy `read_file(n_lines=n)`:
/// lines keep their trailing newline and are joined with an extra `\n`.
/// Returns an empty string when the file has fewer than `n` lines
/// (legacy StopIteration behavior).
fn read_first_lines(content: &str, n: usize) -> String {
    let mut lines = Vec::with_capacity(n);
    let mut rest = content;
    for _ in 0..n {
        match rest.find('\n') {
            Some(i) => {
                lines.push(&rest[..=i]);
                rest = &rest[i + 1..];
            }
            None => {
                if rest.is_empty() {
                    return String::new(); // StopIteration -> ""
                }
                lines.push(rest);
                rest = "";
            }
        }
    }
    lines.join("\n")
}

/// JS-family extensions (legacy `JS_EXTENSIONS`).
const JS_EXTENSIONS: &[&str] = &[".js", ".jsx", ".ts", ".tsx"];
/// C-family extensions (legacy `C_EXTENSIONS`).
const C_EXTENSIONS: &[&str] = &[".c", ".cpp", ".cc", ".h", ".cu", ".cuh", ".hpp"];
const PYTHON_EXTENSIONS: &[&str] = &[".py", ".pyi"];
const MARKDOWN_EXTENSIONS: &[&str] = &[".md", ".markdown", ".mdx", ".mdc"];

/// Extensions recognized by the legacy implementation but deferred in the
/// Rust port version 1. Files still get TODO/BUG/NOTE markers; component
/// extraction is tracked in docs/language-roadmap.md.
const DEFERRED_EXTENSIONS: &[&str] = &[
    ".php", ".kt", ".swift", ".sh", ".ps1", ".zig", ".rb", ".cs", ".jl", ".scala", ".java", ".pl",
    ".hs", ".fs", ".lisp", ".clj", ".scm", ".el", ".rkt", ".erl", ".hrl", ".capnp", ".tex",
    ".lean", ".f", ".for", ".f77", ".f90", ".f95", ".f03", ".f08", ".tf", ".thy", ".lua", ".tcl",
    ".m", ".r", ".nb", ".wl", ".matlab", ".ml", ".cbl", ".cobol", ".apl", ".metal", ".wgsl",
    ".html",
];

/// Whether this extension is deferred (legacy support, no Rust port yet).
pub fn is_deferred_extension(ext: &str) -> bool {
    DEFERRED_EXTENSIONS.contains(&ext)
}

/// Extract displayable component labels from a file.
///
/// `syntax` mirrors the legacy flag: when false, Rust enum components are
/// rich-markup-escaped exactly like the legacy renderer expected.
pub fn extract_components(path: &Path, syntax: bool) -> Vec<String> {
    try_extract_components(path, syntax).unwrap_or_default()
}

fn try_extract_components(path: &Path, syntax: bool) -> ExtractResult {
    let path_str = path.to_string_lossy().replace('\\', "/");
    let (base_path, ext) = splitext_lower(&path_str);
    let file_name = base_path
        .rsplit('/')
        .next()
        .unwrap_or(&base_path)
        .to_string();

    // sqlite databases are handled before reading the file
    if ext == ".db" || ext == ".sqlite" {
        return data::extract_sqlite(path);
    }

    if is_binary(path) {
        return Ok(Vec::new());
    }

    let raw = std::fs::read(path)?;
    let full_content = String::from_utf8_lossy(&raw).into_owned();
    // legacy read_file uses errors="strict": undecodable files read as ""
    let full_content = if std::str::from_utf8(&raw).is_ok() {
        full_content
    } else {
        String::new()
    };

    // big data files only read a few lines
    let content: String = match ext.as_str() {
        ".csv" => read_first_lines(&full_content, 3),
        ".jsonl" => read_first_lines(&full_content, 2),
        _ => full_content,
    };

    let components: Vec<String> = match ext.as_str() {
        e if JS_EXTENSIONS.contains(&e) => {
            if path_str.ends_with(".d.ts") {
                Vec::new() // legacy parse_d_dot_ts returns [] (it never appends)
            } else {
                let mut components =
                    treesitter::typescript::extract(&content, e == ".tsx" || e == ".jsx")?;
                if path_str.contains("spec.ts") {
                    components = simple::angular_spec(&content);
                }
                if path_str.contains("app-routing.module") {
                    let mut routes = simple::angular_routes(&content);
                    routes.extend(components);
                    components = routes;
                } else if path_str.contains("app.module") {
                    let mut module = simple::angular_app_module(&content);
                    module.extend(components);
                    components = module;
                } else if file_name == "environment" || file_name.contains("environment.") {
                    components = simple::environment_ts(&content);
                }
                components
            }
        }
        e if PYTHON_EXTENSIONS.contains(&e) => treesitter::python::extract(&content)?,
        e if MARKDOWN_EXTENSIONS.contains(&e) => markdown::extract_md(&content),
        ".rst" => markdown::extract_rst(&content),
        ".json" => {
            if path_str.to_lowercase().contains("package.json") {
                data::extract_package_json(&content)?
            } else if content.contains("$schema") {
                data::extract_json_schema(&content)?
            } else if content.contains("jsonrpc\": \"2") {
                data::extract_json_rpc(&content)?
            } else if content.contains("openrpc\": \"") {
                data::extract_openrpc_json(&content)?
            } else {
                Vec::new()
            }
        }
        ".yml" | ".yaml" => data::extract_yml(&content)?,
        e if C_EXTENSIONS.contains(&e) => treesitter::c_cpp::extract(&content, e)?,
        ".rs" => treesitter::rust::extract(&content, syntax)?,
        ".go" => treesitter::go::extract(&content)?,
        ".sql" => simple::sql(&content),
        ".graphql" => simple::graphql(&content),
        ".proto" => simple::protobuf(&content),
        ".jsonl" => data::extract_jsonl(&content)?,
        ".env" => simple::dot_env(&content),
        ".txt" => {
            if file_name.contains("requirements") {
                simple::requirements_txt(&content)
            } else {
                markdown::extract_txt(&content)
            }
        }
        ".csv" => data::extract_csv(&content)?,
        "" if file_name == "Makefile" || file_name == "Justfile" => simple::makefile(&content),
        _ if file_name.starts_with(".env") => simple::dot_env(&content),
        _ if path_str.ends_with("Cargo.toml") => data::extract_cargo_toml(&content)?,
        _ if path_str.ends_with("pyproject.toml") => data::extract_pyproject_toml(&content)?,
        _ => Vec::new(),
    };

    // markers skip exactly .txt and .md (legacy quirk: not .markdown/.mdx)
    if ext == ".txt" || ext == ".md" {
        Ok(components)
    } else {
        let mut total = markers::extract_markers(&content);
        total.extend(components);
        Ok(total)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn binary_detection() {
        assert!(is_binary_bytes(&[0x00, 0x01]));
        assert!(!is_binary_bytes(b"hello world\n"));
        assert!(!is_binary_bytes("héllo".as_bytes())); // UTF-8 high bytes are text
        assert!(is_binary_bytes(&[0x7F]));
    }

    #[test]
    fn splitext_examples() {
        assert_eq!(
            splitext_lower("a/b/file.PY"),
            ("a/b/file".to_string(), ".py".to_string())
        );
        assert_eq!(
            splitext_lower(".env.test"),
            (".env".to_string(), ".test".to_string())
        );
        assert_eq!(
            splitext_lower("dir/.env"),
            ("dir/.env".to_string(), String::new())
        );
        assert_eq!(
            splitext_lower("Makefile"),
            ("Makefile".to_string(), String::new())
        );
    }

    #[test]
    fn first_lines_eof_means_empty() {
        assert_eq!(
            read_first_lines("a,b\nc,d\ne,f\ng\n", 3),
            "a,b\n\nc,d\n\ne,f\n"
        );
        assert_eq!(read_first_lines("a,b\n", 3), "");
        assert_eq!(read_first_lines("", 2), "");
    }
}
