//! Tree construction: the Rust analog of `tree_plus_src/engine.py`
//! `from_seeds` / `_map_seeds` / `_from_folder` / `_from_file` / `_from_glob`.

use std::path::{Path, PathBuf};

use rayon::prelude::*;

use crate::config::TreePlusConfig;
use crate::count::count_tokens_lines;
use crate::extract::extract_components;
use crate::ignore::{
    amortize_globs, is_glob, parse_ignore, should_ignore, AmortizedGlobs, IgnorePatterns,
};
use crate::model::{Category, TreePlus};
use crate::sort::os_sort_key;

/// Dedicated rayon pool with large worker stacks. Work-stealing nests
/// `from_folder` frames on a single worker while it waits in `join`, and
/// tree-sitter's C parser needs headroom below that; rayon's default 2 MiB
/// worker stacks segfault on big trees (e.g. torvalds/linux `drivers/`).
fn pool() -> &'static rayon::ThreadPool {
    static POOL: std::sync::LazyLock<rayon::ThreadPool> = std::sync::LazyLock::new(|| {
        rayon::ThreadPoolBuilder::new()
            .stack_size(16 * 1024 * 1024)
            .build()
            .expect("build worker pool")
    });
    &POOL
}

/// Build a `TreePlus` from seed path/glob strings (legacy `from_seeds`).
pub fn from_seeds(seeds: &[String], config: &TreePlusConfig) -> TreePlus {
    pool().install(|| from_seeds_inner(seeds, config))
}

fn from_seeds_inner(seeds: &[String], config: &TreePlusConfig) -> TreePlus {
    let mut seeds: Vec<String> = if seeds.is_empty() {
        vec![std::env::current_dir()
            .map(|p| p.to_string_lossy().into_owned())
            .unwrap_or_else(|_| ".".to_string())]
    } else {
        seeds.to_vec()
    };
    // legacy dedupes via set(); order is later fixed by sorting
    seeds.sort();
    seeds.dedup();

    let ignore = IgnorePatterns::new(parse_ignore(&config.ignore, config.override_ignore));

    // categorize seeds (legacy `_map_seeds`)
    let mut folder_paths: Vec<PathBuf> = Vec::new();
    let mut file_paths: Vec<PathBuf> = Vec::new();
    let mut glob_seed_patterns: Vec<String> = Vec::new();
    let mut filter_globs: Vec<String> = config.globs.clone();
    for seed in &seeds {
        let path = Path::new(seed);
        if path.is_file() {
            file_paths.push(path.to_path_buf());
        } else if path.is_dir() {
            folder_paths.push(path.to_path_buf());
        } else if is_glob(seed) {
            if seed.starts_with("*.") {
                // legacy: bare `*.ext` seeds become filter globs
                filter_globs.push(seed.clone());
            } else {
                glob_seed_patterns.push(seed.clone());
            }
        }
        // non-existent non-glob seeds are skipped with a warning in legacy
    }

    // amortize filter globs (legacy)
    let globs: Option<AmortizedGlobs> = if !filter_globs.is_empty() {
        let glob_roots: Vec<PathBuf> = if folder_paths.is_empty() {
            vec![std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."))]
        } else {
            folder_paths.clone()
        };
        let mut amortized = amortize_globs(&glob_roots, &filter_globs);
        if let Some(a) = amortized.as_mut() {
            // directly-provided files dodge the glob filter
            for f in &file_paths {
                a.matches.insert(f.clone());
            }
        }
        amortized
    } else {
        None
    };
    if !filter_globs.is_empty() && folder_paths.is_empty() && glob_seed_patterns.is_empty() {
        // legacy: globs with no folder seeds scan the cwd
        folder_paths.push(std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));
    }

    // sort all seeds together by natural path order
    enum Seed {
        Folder(PathBuf),
        File(PathBuf),
        GlobPattern(String),
    }
    let mut parsed: Vec<(String, Seed)> = Vec::new();
    for p in folder_paths {
        parsed.push((p.to_string_lossy().into_owned(), Seed::Folder(p)));
    }
    for p in file_paths {
        parsed.push((p.to_string_lossy().into_owned(), Seed::File(p)));
    }
    for g in glob_seed_patterns {
        parsed.push((g.clone(), Seed::GlobPattern(g)));
    }
    parsed.sort_by_key(|a| os_sort_key(&a.0));

    let forest: Vec<TreePlus> = parsed
        .into_iter()
        .map(|(_, seed)| match seed {
            Seed::Folder(p) => from_folder(&p, &ignore, globs.as_ref(), config),
            Seed::File(p) => from_file(&p, config),
            Seed::GlobPattern(g) => from_glob_pattern(&g, &ignore, config),
        })
        .collect();

    match forest.len() {
        0 => TreePlus::new(Category::Root, "No match"),
        1 => forest.into_iter().next().unwrap(),
        _ => {
            let mut root = TreePlus::new(Category::Root, "Root");
            root.subtrees = forest;
            root
        }
    }
}

/// Build a tree from a folder (legacy `_from_folder`).
pub fn from_folder(
    folder_path: &Path,
    ignore: &IgnorePatterns,
    globs: Option<&AmortizedGlobs>,
    config: &TreePlusConfig,
) -> TreePlus {
    // resolve() fixes the `Path(".").name == ""` case
    let name = folder_path
        .canonicalize()
        .ok()
        .and_then(|p| p.file_name().map(|n| n.to_string_lossy().into_owned()))
        .unwrap_or_else(|| folder_path.to_string_lossy().into_owned());
    let mut node = TreePlus::new(Category::Folder, name);

    let mut entries: Vec<PathBuf> = std::fs::read_dir(folder_path)
        .map(|rd| rd.flatten().map(|e| e.path()).collect())
        .unwrap_or_default();
    entries.sort_by_key(|a| os_sort_key(&a.to_string_lossy()));

    // partition first so files can be processed in parallel deterministically
    let kept: Vec<(PathBuf, bool)> = entries
        .into_iter()
        .filter(|p| !should_ignore(p, ignore, globs))
        .filter_map(|p| {
            let is_dir = p.is_dir();
            let is_file = p.is_file();
            if is_dir || is_file {
                Some((p, is_dir))
            } else {
                None
            }
        })
        .collect();

    let subtrees: Vec<TreePlus> = kept
        .into_par_iter()
        .map(|(p, is_dir)| {
            if is_dir {
                from_folder(&p, ignore, globs, config)
            } else {
                from_file(&p, config)
            }
        })
        .collect();
    node.subtrees = subtrees;
    node
}

/// Build a tree from a file (legacy `_from_file`).
pub fn from_file(file_path: &Path, config: &TreePlusConfig) -> TreePlus {
    let counts = count_tokens_lines(file_path).unwrap_or_default();
    let name = file_path
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_else(|| file_path.to_string_lossy().into_owned());
    let mut node = TreePlus::new(Category::File, name);
    node.token_count = counts.n_tokens;
    node.line_count = counts.n_lines;
    if !config.concise && counts.n_tokens <= config.max_tokens {
        node.components = extract_components(file_path, config.syntax);
    }
    node
}

/// Build a tree from a glob-pattern seed like `tree_plus_src/*.py`
/// (legacy `_from_glob`, which uses non-recursive `Path().glob(pattern)`).
pub fn from_glob_pattern(
    pattern: &str,
    ignore: &IgnorePatterns,
    config: &TreePlusConfig,
) -> TreePlus {
    let mut node = TreePlus::new(Category::Glob, pattern);
    let mut matches = glob_relative(Path::new(""), pattern);
    matches.sort_by_key(|a| os_sort_key(&a.to_string_lossy()));
    for m in matches {
        if m.is_dir() {
            node.subtrees.push(from_folder(&m, ignore, None, config));
        } else if m.is_file() {
            node.subtrees.push(from_file(&m, config));
        }
    }
    node
}

/// Minimal `pathlib.Path.glob` for relative patterns: segments are fnmatch
/// globs, `**` matches any number of directories.
fn glob_relative(base: &Path, pattern: &str) -> Vec<PathBuf> {
    use crate::ignore::fnmatch_translate;
    use regex::Regex;

    let segments: Vec<&str> = pattern.split('/').filter(|s| !s.is_empty()).collect();
    let mut current: Vec<PathBuf> = vec![if base.as_os_str().is_empty() {
        PathBuf::from(".")
    } else {
        base.to_path_buf()
    }];
    for (i, segment) in segments.iter().enumerate() {
        let last = i + 1 == segments.len();
        let mut next: Vec<PathBuf> = Vec::new();
        if *segment == "**" {
            for dir in &current {
                next.push(dir.clone());
                collect_dirs_recursive(dir, &mut next);
            }
        } else {
            let Ok(re) = Regex::new(&fnmatch_translate(segment)) else {
                continue;
            };
            for dir in &current {
                let Ok(entries) = std::fs::read_dir(dir) else {
                    continue;
                };
                for entry in entries.flatten() {
                    let name = entry.file_name();
                    let Some(name) = name.to_str() else { continue };
                    if re.is_match(name) {
                        let p = entry.path();
                        if last || p.is_dir() {
                            next.push(p);
                        }
                    }
                }
            }
        }
        current = next;
    }
    // strip the leading "./" to mirror Path().glob output
    current
        .into_iter()
        .map(|p| p.strip_prefix("./").map(Path::to_path_buf).unwrap_or(p))
        .collect()
}

fn collect_dirs_recursive(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let p = entry.path();
        if p.is_dir() {
            out.push(p.clone());
            collect_dirs_recursive(&p, out);
        }
    }
}
