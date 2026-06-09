# Language Roadmap (Rust Port)

Version-1 implements: Rust, Python, JavaScript, TypeScript, C, C++, Go,
Java, Kotlin, Swift,
Markdown (+ RST), JSON (package.json / schema / RPC / OpenRPC), JSONL, YAML,
TOML (Cargo/pyproject), CSV, Makefile/Justfile, .env, requirements.txt,
SQLite, and TODO/BUG/NOTE markers everywhere except `.md`/`.txt` (legacy
rule).

Everything below is recognized by the legacy Python implementation and
**deferred**: files keep counts and markers, but emit no components. Legacy
goldens for every deferred language are checked in under
`tests/golden/legacy/components/` and serve as the acceptance contract.
For each: legacy extension(s) → legacy extractor → tree-sitter grammar
availability → suggested path → missing tests.

| Language | Extensions | Legacy extractor | TS grammar? | Suggested path | Missing tests |
|---|---|---|---|---|---|
| C# | .cs | parse_cs | yes | tree-sitter formatter | port golden `csharp_test.cs` |
| PHP | .php | parse_php | yes | tree-sitter formatter | port golden `php_test.php` |
| Ruby | .rb | parse_rb | yes | tree-sitter formatter | port golden `ruby_test.rb` |
| Scala | .scala | parse_scala | yes (community) | tree-sitter formatter | port golden `scala_test.scala` |
| Julia | .jl | parse_jl | yes (community) | tree-sitter formatter | port golden `JuliaTest.jl` |
| Lua | .lua | parse_lua | yes | tree-sitter formatter | port golden `LuaTest.lua` |
| Haskell | .hs | parse_hs | yes (community) | tree-sitter formatter | port golden `haskell_test.hs` |
| OCaml | .ml | parse_ocaml | yes | tree-sitter formatter | port golden `OcamlTest.ml` |
| F# | .fs | parse_fsharp | partial | regex port (line-oriented) | port golden `fsharp_test.fs` |
| Erlang | .erl .hrl | parse_erl | yes (community) | regex port (attribute lines) | port golden `erl_test.erl` |
| Lisp family | .lisp .clj .scm .el .rkt | parse_lisp | per-dialect | regex port (defun-ish lines) | port group_lisp goldens |
| Perl | .pl | parse_perl | yes (community) | regex port | port golden `perl_test.pl` |
| PowerShell | .ps1 | parse_ps1 | no maintained | regex port | port golden `powershell_test.ps1` |
| Bash | .sh | parse_bash | yes | tree-sitter formatter | port golden `bash_test.sh` |
| Zig | .zig | parse_zig | yes (community) | tree-sitter formatter | port golden `zig_test.zig` |
| R | .r .R | parse_r | yes (community) | regex port | port golden `r_test.r` |
| MATLAB | .m .matlab | parse_matlab | no maintained | regex port + content sniff | port golden `matlab_test.m` |
| Objective-C | .m | parse_objective_c | yes (community) | tree-sitter + content sniff | port golden `objective_c_test.m` |
| Mathematica | .nb .wl | parse_mathematica | no | regex port | port golden `mathematica_test.nb` |
| COBOL | .cbl .cobol | parse_cbl | no maintained | regex port (division/para lines) | port group1 goldens |
| Fortran | .f .f90 ... | parse_fortran | yes (community) | regex port | port golden `fortran_test.f90` |
| APL | .apl | parse_apl | no | regex port | port golden `apl_test.apl` |
| SQL | .sql | parse_sql | yes (community) | regex port (CREATE TABLE) | port golden `sql_test.sql` |
| GraphQL | .graphql | parse_graphql | yes (community) | line port (trivial) | port golden `graphql_test.graphql` |
| Protobuf | .proto | parse_grpc | yes (community) | line/regex port | port golden `proto_test.proto` |
| Cap'n Proto | .capnp | parse_capnp | no | regex port | port golden `capnp_test.capnp` |
| LaTeX | .tex | parse_tex | yes (community) | regex port (sections) | port golden `tex_test.tex` |
| Lean | .lean | parse_lean | community | regex port | port golden `lean_test.lean` |
| Isabelle | .thy | parse_isabelle | no | regex port + symbol table | port golden `isabelle_test.thy` |
| Terraform | .tf | parse_tf | yes (hcl) | regex port | port golden `terraform_test.tf` |
| TCL | .tcl | parse_tcl | community | regex port (proc lines) | port golden `tcl_test.tcl` |
| Metal | .metal | parse_metal | no | regex port | port golden `metal_test.metal` |
| WGSL | .wgsl | parse_wgsl | community | regex port | port golden `wgsl_test.wgsl` |
| HTML | .html | parse_html (returns []) | yes | decide scope first (legacy emitted nothing) | n/a |
| TensorFlow flags | flags-in-tensorflow .cc/.h | parse_tensorflow_flags | n/a | intentionally removed (see differences doc) | n/a |

Also deferred (not languages): web mode (`URL seeds`, `--yc`), syntax
highlighting, tiktoken tokenizers — see docs/rust-port-differences.md.

## Suggested order of attack

1. C#, Ruby, Bash (mature grammars, heavily used).
2. SQL/GraphQL/Protobuf/requirements-style line formats (cheap regex ports).
3. The long tail, prioritized by user demand.

## Implementation notes

- Java and Swift use tree-sitter formatters (`treesitter/java.rs`,
  `treesitter/swift.rs`).
- Kotlin is a procedural line scanner (`extract/kotlin.rs`): the community
  grammar (tree-sitter-kotlin-ng) cannot recover from the deliberately
  invalid constructs in the acceptance fixture — one ERROR node swallows
  the rest of the file — while the legacy pattern is line-anchored and
  keeps going.
