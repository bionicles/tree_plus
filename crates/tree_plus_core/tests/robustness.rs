//! Robustness: arbitrary bytes must never panic, hang, or render
//! nondeterministically. (Deterministic pseudo-random corpus; no fuzz deps.)

use std::io::Write;
use std::path::PathBuf;

use tree_plus_core::extract_components;

/// xorshift64* — deterministic byte stream without external crates.
struct Rng(u64);
impl Rng {
    fn next_u64(&mut self) -> u64 {
        let mut x = self.0;
        x ^= x >> 12;
        x ^= x << 25;
        x ^= x >> 27;
        self.0 = x;
        x.wrapping_mul(0x2545F4914F6CDD1D)
    }
    fn fill(&mut self, buf: &mut [u8]) {
        for chunk in buf.chunks_mut(8) {
            let bytes = self.next_u64().to_le_bytes();
            let n = chunk.len();
            chunk.copy_from_slice(&bytes[..n]);
        }
    }
}

const EXTENSIONS: &[&str] = &[
    "py", "rs", "ts", "tsx", "js", "c", "cpp", "h", "md", "json", "jsonl", "yml", "toml", "csv",
    "txt", "env", "rst",
];

fn write_temp(name: &str, bytes: &[u8]) -> PathBuf {
    let dir = std::env::temp_dir().join("tree_plus_robustness");
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join(name);
    let mut f = std::fs::File::create(&path).unwrap();
    f.write_all(bytes).unwrap();
    path
}

#[test]
fn arbitrary_bytes_never_panic_and_are_deterministic() {
    let mut rng = Rng(0x5EED_CAFE_F00D_BEEF);
    for round in 0..40 {
        let size = (rng.next_u64() % 4096) as usize + 1;
        let mut bytes = vec![0u8; size];
        rng.fill(&mut bytes);
        for ext in EXTENSIONS {
            let path = write_temp(&format!("garbage_{round}.{ext}"), &bytes);
            let first = extract_components(&path, false);
            let second = extract_components(&path, false);
            assert_eq!(first, second, "nondeterministic output for .{ext}");
        }
    }
}

#[test]
fn truncated_real_sources_never_panic() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
    let sources = [
        "tests/more_languages/group4/rust_test.rs",
        "tests/path_to_test/class_method_type.py",
        "tests/more_languages/group1/test.ts",
        "tests/more_languages/group3/cpp_test.cpp",
        "tests/more_languages/group2/test.csv",
    ];
    for rel in sources {
        let bytes = std::fs::read(root.join(rel)).unwrap();
        let ext = rel.rsplit('.').next().unwrap();
        // cut at byte boundaries, including mid-UTF-8 and mid-token
        for cut in [1, 7, bytes.len() / 3, bytes.len() / 2, bytes.len() - 1] {
            let cut = cut.min(bytes.len());
            let path = write_temp(&format!("truncated_{cut}.{ext}"), &bytes[..cut]);
            let _ = extract_components(&path, false); // must not panic
        }
    }
}

#[test]
fn invalid_utf8_yields_no_components() {
    // legacy read_file used strict UTF-8 and returned "" on decode errors
    let path = write_temp("invalid.py", &[0xC3, 0x28, b'\n', b'd', b'e', b'f', b' ']);
    assert!(extract_components(&path, false).is_empty());
}
