//! CLI integration tests mirroring the README examples (local fs mode).

use std::path::Path;
use std::process::Command;

fn repo_root() -> std::path::PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap()
}

fn tree_plus(args: &[&str]) -> std::process::Output {
    Command::new(env!("CARGO_BIN_EXE_tree_plus"))
        .args(args)
        .current_dir(repo_root())
        .output()
        .expect("run tree_plus")
}

#[test]
fn version_flag() {
    let out = tree_plus(&["-v"]);
    assert!(out.status.success());
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(stdout.trim().starts_with("2."), "version: {stdout}");
}

#[test]
fn renders_a_directory_with_stats_footer() {
    let out = tree_plus(&["tests/path_to_test"]);
    assert!(out.status.success());
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(stdout.contains("📁 path_to_test (1 folder, 6 files)"));
    assert!(stdout.contains("└── def hello_world()"));
    assert!(stdout.contains("folder(s), 6 file(s),"));
    assert!(stdout.contains("second(s)."));
}

#[test]
fn ignore_flag_excludes_extension() {
    let out = tree_plus(&["-i", "*.py", "tests/path_to_test"]);
    assert!(out.status.success());
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(!stdout.contains(".py ("));
    assert!(stdout.contains("file.md"));
}

#[test]
fn glob_flag_filters() {
    let out = tree_plus(&["-g", "*.rs", "tests/more_languages"]);
    assert!(out.status.success());
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(stdout.contains(".rs ("));
    assert!(!stdout.contains(".java"));
}

#[test]
fn concise_mode_omits_components() {
    let out = tree_plus(&["-c", "tests/path_to_test"]);
    assert!(out.status.success());
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(stdout.contains("📁 path_to_test"));
    assert!(!stdout.contains("def hello_world()"));
    assert!(stdout.contains("concise=True"));
}

#[test]
fn glob_seed_pattern() {
    let out = tree_plus(&["tree_plus_src/*.py"]);
    assert!(out.status.success());
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(stdout.contains("🌀 tree_plus_src/*.py ("));
    assert!(stdout.contains("engine.py"));
}

#[test]
fn unsupported_tokenizer_errors_explicitly() {
    let out = tree_plus(&["-t", "tests/path_to_test"]);
    assert!(!out.status.success());
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("not supported"));
}

#[test]
fn renders_this_repository() {
    let out = tree_plus(&["-c", "."]);
    assert!(out.status.success());
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(stdout.contains("📁 tree_plus ("));
    assert!(stdout.contains("README.md"));
}

#[test]
fn json_flag_emits_machine_readable_tree() {
    let out = tree_plus(&["--json", "tests/path_to_test"]);
    assert!(out.status.success());
    let stdout = String::from_utf8_lossy(&out.stdout);
    let value: serde_json::Value = serde_json::from_str(&stdout).expect("valid JSON");
    assert_eq!(value["category"], "folder");
    assert_eq!(value["name"], "path_to_test");
    assert_eq!(value["n_files"], 6);
    // no footer noise after the JSON document
    assert!(!stdout.contains("second(s)."));
    // a file node carries its components
    let files = value["subtrees"].as_array().expect("subtrees");
    let py = files
        .iter()
        .find(|f| f["name"] == "file.py")
        .expect("file.py node");
    assert_eq!(py["category"], "file");
    assert_eq!(
        py["components"].as_array().expect("components")[0],
        "def hello_world()"
    );
    assert!(py.get("subtrees").is_none());
}

#[test]
fn json_flag_respects_concise() {
    let out = tree_plus(&["--json", "--concise", "tests/path_to_test"]);
    assert!(out.status.success());
    let value: serde_json::Value =
        serde_json::from_str(&String::from_utf8_lossy(&out.stdout)).expect("valid JSON");
    let files = value["subtrees"].as_array().expect("subtrees");
    let py = files
        .iter()
        .find(|f| f["name"] == "file.py")
        .expect("file.py");
    assert_eq!(py["components"].as_array().expect("components").len(), 0);
}
