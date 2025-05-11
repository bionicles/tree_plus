# Tree Plus

#### **Ever hit context limits prompting LLMs with code?**
**If so, you could benefit from `tree_plus`, a `tree` util enhanced with tokens, lines, and components.**

### Use `tree_plus` to ...:
- **Summarize huge codebases easily**
- **Autogenerate API documentation**
- **Create ideal LLM prompts**

[![License](https://img.shields.io/badge/license-MIT%2FApache--2.0-brightgreen)](https://choosealicense.com/licenses/)
[![Docs](https://img.shields.io/badge/docs-README-blue)](https://github.com/bionicles/tree_plus/blob/main/README.md)
[![Linux & MacOS](https://github.com/bionicles/tree_plus/actions/workflows/unix.yml/badge.svg)](https://github.com/bionicles/tree_plus/actions/workflows/unix.yml)
[![Microsoft](https://github.com/bionicles/tree_plus/actions/workflows/microsoft.yml/badge.svg)](https://github.com/bionicles/tree_plus/actions/workflows/microsoft.yml)

**Supported Python Versions:**

![Python 3.8](https://img.shields.io/badge/Python-3.8-blue)
![Python 3.9](https://img.shields.io/badge/Python-3.9-blue)
![Python 3.10](https://img.shields.io/badge/Python-3.10-blue)
![Python 3.11](https://img.shields.io/badge/Python-3.11-blue)
![Python 3.12](https://img.shields.io/badge/Python-3.12-blue)
![Python 3.12](https://img.shields.io/badge/Python-3.13-blue)

**Supported Operating Systems:**

![Ubuntu](https://img.shields.io/badge/Supports-Ubuntu-orange)
![MacOS](https://img.shields.io/badge/Supports-MacOS-orange)
![Windows](https://img.shields.io/badge/Supports-Windows-orange)

**Support Free, Open-Source Software:**

[![Support Tree Plus](https://img.shields.io/badge/Support%20Tree%20Plus-8A2BE2)](https://www.buymeacoffee.com/bionicles)


`pip install -U tree_plus`

## Usage
Here's how `tree_plus --help` looks (`-h` and `-H` both also work) 
<!-- t5-start -->
```sh
tree_plus -h
Usage: tree_plus [OPTIONS] [PATHS]...

  A `tree` util enhanced with tokens, lines, and components.

  Wrap patterns in quotes: -i "*.py" / -g "*.rs"

  Example Invocations:

          Show tree_plus_src and tests simultaneously
              > tree_plus tree_plus_src tests

          Show files matching "*.*s" within tests/more_languages
              > tree_plus -g "*.*s" tests/more_languages

          Ignore Java files
              > tree_plus -i "*.java" tests

          Override DEFAULT_IGNORE: Only ignore .ini files.
              > tree_plus -o -i "*.ini" tests/dot_dot

          Syntax Highlight python files in src and tests
              > tree_plus -s tree_plus_src/*.py tests/*.py

          Concise Mode (No Parsing)
              > tree_plus -c

          URL + Tag Categories for a website
              > tree_plus example.com

          URL + Tag Categories for multiple websites with a link tree
              > tree_plus example.com example.org -l

          Hacker News Mode (3 articles, max depth 3)
              > tree_plus --yc

          Hacker News Mode (6 articles, max depth 6, warning, slow!)
              > tree_plus --yc -n 6 -m 6

          Use the Tiktoken gpt4o Model Tokenizer to tokenize Rust files
              > tree_plus -t -g '*.rs'

Options:
  -i, -I, --ignore TEXT        Patterns to ignore, in quotes: -i "*.java"
  -o, -O, --override           Override DEFAULT_IGNORE (includes ignored
                               content): -o -i "*.java"
  -g, -G, --glob TEXT          Patterns to find, in quotes: -g "*.rs"
  -v, -V, --version            Print the version and exit.
  -d, -D, --debug              Enables $DEBUG_TREE_PLUS.
  -s, -S, --syntax             Enables Syntax Highlighting (WIP).
  -c, -C, --concise            Omit module components. (False)
  --yc, --hn                   Include ycombinator (False)
  -n, -N, --number INTEGER     number of results (--yc mode only, default 3)
  -m, -M, --max-depth INTEGER  max number of steps (depth / level) from root
                               (--yc mode only, default 3)
  -l, -L, --links              include links (web mode only, default False)
  -t, --tiktoken               a shorthand for tiktoken with the gpt4o
                               tokenizer
  -T, --tokenizer-name TEXT    name of the tokenizer to use, for now only
                               'gpt4o' works
  --timeout FLOAT              regex timeout in seconds (optional, default
                               0.7)
  -H, -h, --help               Show this message and exit.

  v(1.0.61) --- https://github.com/bionicles/tree_plus/blob/main/README.md

```
<!-- t5-end -->


## Example Output:
- [ ] Demonstrate Parsed Checkboxes
<!-- t1-start -->
```sh
tree_plus -i tests
📁 tree_plus (7 folders, 27 files) 
├── 📄 .env.test (4 tokens, 0 lines)
│   └── DEBUG_TREE_PLUS
├── 📁 .github (2 folders, 3 files) 
│   ├── 📄 dependabot.yml (128 tokens, 11 lines)
│   │   └── Unsupported YAML Category
│   └── 📁 workflows (1 folder, 2 files) 
│       ├── 📄 microsoft.yml (284 tokens, 40 lines)
│       │   ├── Microsoft
│       │   ├──   job: build
│       │   ├──     - Set up Python ${{ matrix.python-version }}
│       │   ├──     - Install tree_plus
│       │   ├──     - Create .env file
│       │   ├──     - Set PYTHONUTF8 for Windows
│       │   ├──     - Run generic tests
│       │   └──     - Run specific test
│       └── 📄 unix.yml (664 tokens, 87 lines)
│           ├── Linux & MacOS
│           ├──   job: test
│           ├──     - Set up Python ${{ matrix.python-version }}
│           ├──     - Install tree_plus
│           ├──     - Create .env file
│           ├──     - Run generic tests
│           ├──     - Run specific test
│           ├──   job: deploy
│           ├──     - Set up Python
│           ├──     - Install dependencies
│           ├──     - Increment Version
│           ├──     - Build
│           ├──     - Install
│           ├──     - Test
│           ├──     - Update README
│           ├──     - Build Again
│           ├──     - Commit Updates
│           └──     - Publish to PyPI
├── 📄 .gitignore (202 tokens, 52 lines)
├── 📁 coverage (1 folder, 1 file) 
│   └── 📄 lcov.info (17,359 tokens, 2,180 lines)
├── 📄 LICENSE (2,744 tokens, 81 lines)
├── 📄 Makefile (770 tokens, 121 lines)
│   ├── SHELL := /bin/bash
│   ├── cli
│   ├── library-demo
│   ├── rewrite-demo
│   ├── coverage
│   ├── debug
│   ├── .PHONY: debug-command
│   ├── debug-command: test
│   ├── html-demo
│   ├── absurdly-huge-jsonl
│   ├── test: test-sequential test-tp-dotdot test-e2e test-cli test-programs test-deploy
│   ├── test-parallel
│   ├── test-sequential
│   ├── test-more-languages
│   ├── test-group
│   ├── test-units
│   ├── test-tp-dotdot
│   ├── test-e2e
│   ├── test-cli: cli
│   ├── test-programs
│   ├── test-deploy
│   ├── vulture: install_vulture
│   ├── install-vulture
│   ├── build: install-build-tool clean-dist
│   ├── install-wheel
│   ├── install-build-tool
│   ├── test-publish: install-twine
│   ├── install-twine
│   ├── publish: install-twine
│   ├── clean-dist
│   ├── t1
│   ├── t2
│   ├── t3
│   ├── t4
│   ├── t5
│   └── t6
├── 📄 nodemon.json (112 tokens, 24 lines)
├── 📄 pyproject.toml (364 tokens, 51 lines)
│   ├── name: tree_plus
│   ├── version: N/A
│   ├── description: A `tree` util enhanced with tokens, lines, and components.
│   ├── License :: OSI Approved :: Apache Software License
│   ├── License :: OSI Approved :: MIT License
│   ├── dependencies:
│   ├──     tiktoken
│   ├──     PyYAML
│   ├──     click
│   ├──     rich
│   ├──     tomli
│   ├──     natsort>=7.1
│   ├──     fake_useragent
│   ├──     bs4
│   ├──     func_timeout
│   └──     regex
├── 📄 pytest.ini (20 tokens, 4 lines)
├── 📄 README.md (38,064 tokens, 3,733 lines)
│   ├── TODO: research various kwargs for huggingface
│   ├── TODO: fix this path
│   ├── TODO: fill in these stubs
│   ├── TODO: fix this path
│   ├── TODO: fill in these stubs
│   ├── TODO: show off how well we parse_todo
│   ├── TODO: MOVE TIMEOUT_SECONDS TO ENV VAR
│   ├── NOTE: you can customize the color here
│   ├── TODO: clarify subtree types
│   ├── NOTE: sometimes we need wider trees
│   ├── TODO: research
│   ├── NOTE: here we add directly input file_paths to the amortized glob matches
│   ├── TODO: decide if we apply glob patterns to glob paths
│   ├── NOTE: switching these eager tallies to lazy properties
│   ├── TODO: decide between glob and rglob in _from_glob
│   ├── TODO: decide if we need to re
│   ├── TODO: clarify ignore in glob seed context
│   ├── TODO: re
│   ├── NOTE: this is only to satisfy the type checker
│   ├── TODO: incorporate gitignore
│   ├── BUG: HTML tree doesn
│   ├── TODO: Fix HTML in TreePlus
│   ├── BUG: this repeatedly finds tags
│   ├── BUG: catastrophic backtracking in some c files
│   ├── TODO: update parse_objective_c to avoid fixed unrolling
│   ├── TODO: re
│   ├── NOTE: no point in the answers since there
│   ├── TODO: This todo tests parse_todo
│   ├── TODO: This todo tests parse_todo
│   ├── TODO: research various kwargs for huggingface
│   ├── TODO: fix this path
│   ├── TODO: fill in these stubs
│   ├── TODO: fix this path
│   ├── TODO: fill in these stubs
│   ├── TODO: show off how well we parse_todo
│   ├── TODO: MOVE TIMEOUT_SECONDS TO ENV VAR
│   ├── NOTE: you can customize the color here
│   ├── TODO: clarify subtree types
│   ├── NOTE: sometimes we need wider trees
│   ├── TODO: research
│   ├── NOTE: directly add input file_paths to the amortized glob matches
│   ├── TODO: decide to apply glob patterns to glob paths
│   ├── TODO: decide between glob and rglob in _from_glob
│   ├── TODO: decide if we need to re
│   ├── TODO: clarify ignore in glob seed context
│   ├── NOTE: this is only to satisfy the type checker
│   ├── TODO: incorporate gitignore
│   ├── BUG: HTML tree doesn
│   ├── TODO: Fix HTML in TreePlus
│   ├── BUG: this repeatedly finds tags
│   ├── TODO: update parse_objective_c to avoid fixed unrolling
│   ├── TODO: update parse_ocaml to avoid forced unrolling
│   ├── TODO: fix parse_apl to avoid forced unrolling
│   ├── TODO: fix parse_perl to avoid forced unrolling
│   ├── TODO: re
│   ├── NOTE: no point in the answers since there
│   ├── TODO: This todo tests parse_todo
│   ├── TODO: technically we should build this module without AVX support
│   ├── TODO: This todo tests parse_todo
│   ├── TODO: research various kwargs for huggingface
│   ├── TODO: fix this path
│   ├── TODO: fill in these stubs
│   ├── TODO: fix this path
│   ├── TODO: fill in these stubs
│   ├── TODO: show off how well we parse_todo
│   ├── TODO: MOVE TIMEOUT_SECONDS TO ENV VAR
│   ├── NOTE: you can customize the color here
│   ├── TODO: clarify subtree types
│   ├── NOTE: sometimes we need wider trees
│   ├── TODO: research
│   ├── NOTE: directly add input file_paths to the amortized glob matches
│   ├── TODO: decide to apply glob patterns to glob paths
│   ├── TODO: decide between glob and rglob in _from_glob
│   ├── TODO: decide if we need to re
│   ├── TODO: clarify ignore in glob seed context
│   ├── NOTE: this is only to satisfy the type checker
│   ├── TODO: incorporate gitignore
│   ├── BUG: HTML tree doesn
│   ├── TODO: Fix HTML in TreePlus
│   ├── BUG: this repeatedly finds tags
│   ├── TODO: update parse_objective_c to avoid fixed unrolling
│   ├── TODO: update parse_ocaml to avoid forced unrolling
│   ├── TODO: fix parse_apl to avoid forced unrolling
│   ├── TODO: fix parse_perl to avoid forced unrolling
│   ├── TODO: re
│   ├── NOTE: no point in the answers since there
│   ├── TODO: This todo tests parse_todo
│   ├── TODO: technically we should build this module without AVX support
│   ├── TODO: This todo tests parse_todo
│   ├── TODO: research various kwargs for huggingface
│   ├── TODO: fix this path
│   ├── TODO: fill in these stubs
│   ├── TODO: fix this path
│   ├── TODO: fill in these stubs
│   ├── TODO: show off how well we parse_todo
│   ├── TODO: MOVE TIMEOUT_SECONDS TO ENV VAR
│   ├── NOTE: you can customize the color here
│   ├── TODO: clarify subtree types
│   ├── NOTE: sometimes we need wider trees
│   ├── TODO: research
│   ├── NOTE: directly add input file_paths to the amortized glob matches
│   ├── TODO: decide to apply glob patterns to glob paths
│   ├── TODO: decide between glob and rglob in _from_glob
│   ├── TODO: decide if we need to re
│   ├── TODO: clarify ignore in glob seed context
│   ├── NOTE: this is only to satisfy the type checker
│   ├── TODO: incorporate gitignore
│   ├── BUG: HTML tree doesn
│   ├── TODO: Fix HTML in TreePlus
│   ├── BUG: this repeatedly finds tags
│   ├── TODO: update parse_objective_c to avoid fixed unrolling
│   ├── TODO: update parse_ocaml to avoid forced unrolling
│   ├── TODO: fix parse_apl to avoid forced unrolling
│   ├── TODO: fix parse_perl to avoid forced unrolling
│   ├── TODO: re
│   ├── NOTE: no point in the answers since there
│   ├── TODO: This todo tests parse_todo
│   ├── TODO: technically we should build this module without AVX support
│   ├── TODO: This todo tests parse_todo
│   ├── TODO: research various kwargs for huggingface
│   ├── TODO: fix this path
│   ├── TODO: fill in these stubs
│   ├── TODO: fix this path
│   ├── TODO: fill in these stubs
│   ├── TODO: show off how well we parse_todo
│   ├── TODO: MOVE TIMEOUT_SECONDS TO ENV VAR
│   ├── NOTE: you can customize the color here
│   ├── TODO: clarify subtree types
│   ├── NOTE: sometimes we need wider trees
│   ├── TODO: research
│   ├── NOTE: directly add input file_paths to the amortized glob matches
│   ├── TODO: decide to apply glob patterns to glob paths
│   ├── TODO: decide between glob and rglob in _from_glob
│   ├── TODO: decide if we need to re
│   ├── TODO: clarify ignore in glob seed context
│   ├── NOTE: this is only to satisfy the type checker
│   ├── TODO: incorporate gitignore
│   ├── BUG: HTML tree doesn
│   ├── TODO: Fix HTML in TreePlus
│   ├── BUG: this repeatedly finds tags
│   ├── TODO: gather requirements for C namespace visualization
│   ├── TODO: update parse_objective_c to avoid fixed unrolling
│   ├── TODO: update parse_ocaml to avoid forced unrolling
│   ├── TODO: fix parse_apl to avoid forced unrolling
│   ├── TODO: fix parse_perl to avoid forced unrolling
│   ├── TODO: re
│   ├── NOTE: no point in the answers since there
│   ├── TODO: This todo tests parse_todo
│   ├── TODO: technically we should use a proper parser
│   ├── TODO: This todo tests parse_todo
│   ├── TODO: research various kwargs for huggingface
│   ├── TODO: fix this path
│   ├── TODO: fill in these stubs
│   ├── TODO: fix this path
│   ├── TODO: fill in these stubs
│   ├── TODO: show off how well we parse_todo
│   ├── TODO: MOVE TIMEOUT_SECONDS TO ENV VAR
│   ├── NOTE: you can customize the color here
│   ├── TODO: clarify subtree types
│   ├── NOTE: sometimes we need wider trees
│   ├── TODO: research
│   ├── NOTE: directly add input file_paths to the amortized glob matches
│   ├── TODO: decide to apply glob patterns to glob paths
│   ├── TODO: decide between glob and rglob in _from_glob
│   ├── TODO: decide if we need to re
│   ├── TODO: clarify ignore in glob seed context
│   ├── NOTE: this is only to satisfy the type checker
│   ├── TODO: incorporate gitignore
│   ├── BUG: HTML tree doesn
│   ├── TODO: Fix HTML in TreePlus
│   ├── BUG: this repeatedly finds tags
│   ├── TODO: gather requirements for C namespace visualization
│   ├── TODO: update parse_objective_c to avoid fixed unrolling
│   ├── TODO: update parse_ocaml to avoid forced unrolling
│   ├── TODO: fix parse_apl to avoid forced unrolling
│   ├── TODO: fix parse_perl to avoid forced unrolling
│   ├── TODO: re
│   ├── NOTE: no point in the answers since there
│   ├── TODO: This todo tests parse_todo
│   ├── TODO: technically we should use a proper parser
│   ├── TODO: This todo tests parse_todo
│   ├── TODO: research various kwargs for huggingface
│   ├── TODO: fix this path
│   ├── TODO: fill in these stubs
│   ├── TODO: fix this path
│   ├── TODO: fill in these stubs
│   ├── TODO: show off how well we parse_todo
│   ├── TODO: MOVE TIMEOUT_SECONDS TO ENV VAR
│   ├── NOTE: you can customize the color here
│   ├── TODO: clarify subtree types
│   ├── NOTE: sometimes we need wider trees
│   ├── TODO: research
│   ├── NOTE: directly add input file_paths to the amortized glob matches
│   ├── TODO: decide to apply glob patterns to glob paths
│   ├── TODO: decide between glob and rglob in _from_glob
│   ├── TODO: decide if we need to re
│   ├── TODO: clarify ignore in glob seed context
│   ├── NOTE: this is only to satisfy the type checker
│   ├── TODO: incorporate gitignore
│   ├── BUG: HTML tree doesn
│   ├── TODO: Fix HTML in TreePlus
│   ├── BUG: this repeatedly finds tags
│   ├── TODO: gather requirements for C namespace visualization
│   ├── TODO: update parse_objective_c to avoid fixed unrolling
│   ├── TODO: update parse_ocaml to avoid forced unrolling
│   ├── TODO: fix parse_apl to avoid forced unrolling
│   ├── TODO: fix parse_perl to avoid forced unrolling
│   ├── TODO: re
│   ├── NOTE: no point in the answers since there
│   ├── TODO: This todo tests parse_todo
│   ├── TODO: technically we should use a proper parser
│   ├── TODO: This todo tests parse_todo
│   ├── TODO: research various kwargs for huggingface / torch performance
│   ├── TODO: fix this path",
│   ├── TODO: fill in these stubs:",
│   ├── TODO: fix this path
│   ├── TODO: fill in these stubs:
│   ├── TODO: show off how well we parse_todo!
│   ├── TODO: MOVE TIMEOUT_SECONDS TO ENV VAR & CLI INPUT
│   ├── NOTE: you can customize the color here, and we could make this functional
│   ├── TODO: clarify subtree types -- make this a DataFrame tbh
│   ├── NOTE: sometimes we need wider trees (deploy)
│   ├── TODO: research & decide about globs as paths instead of as filters
│   ├── NOTE: directly add input file_paths to the amortized glob matches
│   ├── TODO: decide to apply glob patterns to glob paths (currently NO)
│   ├── TODO: decide between glob and rglob in _from_glob
│   ├── TODO: decide if we need to re-amortize the globs in the glob seed
│   ├── TODO: clarify ignore in glob seed context, skipping for now
│   ├── NOTE: this is only to satisfy the type checker
│   ├── TODO: incorporate gitignore
│   ├── BUG: HTML tree doesn't look awesome (yet)
│   ├── TODO: Fix HTML in TreePlus (How?)
│   ├── BUG: this repeatedly finds tags, need to avoid repeating ourselves
│   ├── TODO: gather requirements for C namespace visualization
│   ├── TODO: update parse_objective_c to avoid fixed unrolling
│   ├── TODO: update parse_ocaml to avoid forced unrolling
│   ├── TODO: fix parse_apl to avoid forced unrolling
│   ├── TODO: fix parse_perl to avoid forced unrolling
│   ├── TODO: re-enable tree plus web actions
│   ├── NOTE: no point in the answers since there's no content
│   ├── TODO: This todo tests parse_todo
│   ├── TODO: technically we should use a proper parser
│   ├── TODO: This todo tests parse_todo
│   ├── # Tree Plus
│   ├── #### **Ever hit context limits prompting LLMs with code?**
│   ├── ### Use `tree_plus` to ...:
│   ├── ## Usage
│   ├── ## Example Output:
│   ├── - [ ] Demonstrate Parsed Checkboxes
│   ├── ## Start Quick!
│   ├── ### Prerequisites
│   ├── ### Install Tree Plus
│   ├── #### PyPI
│   ├── #### Local Hackable Install
│   ├── ### Alias Usage
│   ├── ## Library Usage:
│   ├── ## Moar Languages
│   ├── ### Currently Tested Languages:
│   ├── ### Without the -c "Concise" Flag:
│   ├── ## Got Globs?
│   ├── ## Languages Todo:
│   ├── ## Oppose Unfair Business Practices
│   └── ## License
├── 📄 tree_plus_cli.py (2,283 tokens, 331 lines)
│   ├── def move_cwd_to_end_of_sys_path()
│   └── def main(
│           glob: Optional[Tuple],
│           paths: Optional[Union[str, Tuple]],
│           ignore: Tuple,
│           override: bool,
│           debug: bool,
│           version: bool,
│           syntax: bool,
│           concise: bool,
│       
│           yc: bool,
│           number: int,
│           max_depth: int,
│           links: bool,
│           tiktoken: bool,
│           tokenizer_name: Optional,
│           timeout: Optional,
│       )
├── 📁 tree_plus_programs (1 folder, 4 files) 
│   ├── 📄 hello_tree_plus.py (545 tokens, 80 lines)
│   │   ├── class ItHappened
│   │   ├── def main()
│   │   └── def trees(
│   │           or_: bool,
│   │           it: int,
│   │           didnt: str,
│   │           happen: tuple,
│   │       ) -> ItHappened
│   ├── 📄 rewrite.py (4,017 tokens, 471 lines)
│   │   ├── TODO: research various kwargs for huggingface / torch performance
│   │   ├── 
│   │   │   DEFAULT_MAX_LENGTH = 256
│   │   ├── class ModelName(Enum)
│   │   ├──     QWEN_2 = "Qwen/Qwen2-7B-Instruct"
│   │   ├──     MISTRAL_3 = "mistralai/Mistral-7B-Instruct-v0.3"
│   │   ├── class ModelContext(Enum)
│   │   ├──     QWEN_2 = 131
│   │   ├──     MISTRAL_3 = 32
│   │   ├──             root: None,
│   │   ├── def rewrite_module(
│   │   │       max_length: int,
│   │   │       fill_context: bool,
│   │   │       input_path: Path,
│   │   │       output_path: Optional,
│   │   │       suffix: Optional = ".rs",
│   │   │       include_complete_file: bool = True,
│   │   │   )
│   │   └── def main(
│   │           fill_context: bool,
│   │           length: int,
│   │           input_path: Path,
│   │           output_path: Optional,
│   │       )
│   ├── 📄 stub_tests.py (1,348 tokens, 180 lines)
│   │   ├── TODO: fix this path",
│   │   ├── TODO: fill in these stubs:",
│   │   ├── @lru_cache
│   │   │   def remove_decorators(component: str) -> str
│   │   ├── def make_import_path(path: Path) -> str
│   │   ├── def stub_tests(
│   │   │       input_path: str,
│   │   │       output_path: Optional,
│   │   │       rewrite_ok: bool,
│   │   │   ) -> Tuple[List, str, bool]
│   │   ├── def main(
│   │   │       input_path: str,
│   │   │       output_path: Optional,
│   │   │       rewrite_ok: bool,
│   │   │   )
│   │   ├── class Vehicle
│   │   └── class Car(Vehicle)
│   └── 📄 test_stub_tests.py (79 tokens, 20 lines)
│       ├── TODO: fix this path
│       ├── TODO: fill in these stubs:
│       ├── def test_remove_decorators()
│       ├── def test_make_import_path()
│       ├── def test_stub_tests()
│       ├── def test_main()
│       ├── def test_class_vehicle()
│       └── def test_class_car()
└── 📁 tree_plus_src (2 folders, 10 files) 
    ├── 📄 count_tokens_lines.py (1,317 tokens, 209 lines)
    │   ├── TODO: show off how well we parse_todo!
    │   ├── @dataclass(frozen=True)
    │   │   class TokenLineCount
    │   ├──     n_tokens: int
    │   ├──     n_lines: int
    │   ├── class TokenizerName(Enum)
    │   ├──     WC = "wc"
    │   ├──     GPT4O = "gpt-4o"
    │   ├──     GPT4 = "gpt-4"
    │   ├── def count_tokens_lines(
    │   │       file_path: Union,
    │   │       *,
    │   │       tokenizer_name: TokenizerName = TokenizerName.WC,
    │   │   ) -> Optional[TokenLineCount]
    │   ├── def count_openai_tokens_lines_from_contents(
    │   │       contents: Union,
    │   │       *,
    │   │       tokenizer_name: TokenizerName = TokenizerName.GPT4,
    │   │   ) -> TokenLineCount
    │   ├── def count_wc_tokens_lines_from_path(file_path: str) -> TokenLineCount
    │   └── def add_tokens_lines(
    │           lhs_count: TokenLineCount, rhs_count: TokenLineCount
    │       ) -> TokenLineCount
    ├── 📄 debug.py (186 tokens, 39 lines)
    │   ├── def disable_debug()
    │   ├── @lru_cache
    │   │   def debug_enabled()
    │   ├── def debug_print(*args, **kwargs)
    │   ├── def enable_debug()
    │   └── @contextmanager
    │       def debug_disabled()
    ├── 📄 deploy.py (2,058 tokens, 230 lines)
    │   ├── def extract(path: Optional = None) -> str
    │   ├── def load(content: Optional = None, path: Optional = None)
    │   ├── def extract_version(source_path: Optional = None) -> Tuple
    │   ├── def increment_version(
    │   │       source_path: Optional = None,
    │   │       sink_path: Optional = None,
    │   │   )
    │   ├── def run_command(command: Optional = None, debug: bool = False)
    │   ├── def replace_readme_section(
    │   │       source_path: Optional = None,
    │   │       sink_path: Optional = None,
    │   │       marker: Optional = None,
    │   │       command: Optional = None,
    │   │   )
    │   ├── def update_readme(source_path: Optional = None, sink_path: Optional = None)
    │   └── def main()
    ├── 📄 engine.py (12,006 tokens, 1,433 lines)
    │   ├── TODO: MOVE TIMEOUT_SECONDS TO ENV VAR & CLI INPUT
    │   ├── NOTE: you can customize the color here, and we could make this functional
    │   ├── TODO: clarify subtree types -- make this a DataFrame tbh
    │   ├── NOTE: sometimes we need wider trees (deploy)
    │   ├── TODO: research & decide about globs as paths instead of as filters
    │   ├── NOTE: directly add input file_paths to the amortized glob matches
    │   ├── TODO: decide to apply glob patterns to glob paths (currently NO)
    │   ├── TODO: decide between glob and rglob in _from_glob
    │   ├── TODO: decide if we need to re-amortize the globs in the glob seed
    │   ├── TODO: clarify ignore in glob seed context, skipping for now
    │   ├── NOTE: this is only to satisfy the type checker
    │   ├── 
    │   │   INTO_RICH_TREE_TIMEOUT_SECONDS = 1
    │   ├── 
    │   │   BLACK = "black"
    │   ├── class Category(Enum)
    │   ├── 
    │   │       ROOT = 1
    │   ├──     GLOB = 2
    │   ├──     FOLDER = 3
    │   ├──     FILE = 4
    │   ├──     COMPONENT = 5
    │   ├──     URL = 6
    │   ├──     TAG = 7
    │   ├── @dataclass
    │   │   class TreePlus
    │   ├──     category: Category
    │   ├──     name: Union
    │   ├──     line_count: int
    │   ├──     token_count: int
    │   ├──     subtrees: Union[
    │   ├──     hrefs: Optional[Dict]
    │   ├──     @property
    │   │       def has_tree_plus_subtrees(self) -> Optional
    │   ├──     @property
    │   │       def n_folders(self) -> int
    │   ├──     @property
    │   │       def n_files(self) -> int
    │   ├──     @property
    │   │       def n_lines(self) -> int
    │   ├──     @property
    │   │       def n_tokens(self) -> int
    │   ├──     def is_root(self) -> bool
    │   ├──     def is_folder(self) -> bool
    │   ├──     def is_file(self) -> bool
    │   ├──     def is_glob(self) -> bool
    │   ├──     def is_component(self) -> bool
    │   ├──     def is_url(self) -> bool
    │   ├──     def into_rich_tree(self) -> Tree
    │   ├──     def into_str(self) -> str
    │   ├──     def render(
    │   │           self,
    │   │           style: Optional = None,
    │   │           highlight: bool = False,
    │   │           markup: bool = True,
    │   │           capturing: bool = False,
    │   │       )
    │   ├──     def render_hrefs(self)
    │   ├──     def stats(self) -> str
    │   ├── def from_hrefs(
    │   │       hrefs: dict,
    │   │       root_panel_text,
    │   │       link_color: str = LINK_COLOR,
    │   │   ) -> Optional[TreePlus]
    │   ├── def stats_from_tree_plus(tree: TreePlus) -> str
    │   ├── @lru_cache
    │   │   def remove_trailing_space(x: str) -> str
    │   ├── def tree_to_string(
    │   │       tree: Tree,
    │   │       markup: bool = False,
    │   │       highlight: bool = False,
    │   │       text_style: str = TEXT_COLOR,
    │   │   ) -> str
    │   ├── def clean_string(input_str: str) -> str
    │   ├── def safe_print(
    │   │       tree: Union[Tree, Panel, str],
    │   │       style: Optional = None,
    │   │       highlight: bool = True,
    │   │       markup: bool = False,
    │   │       capturing: bool = False,
    │   │   )
    │   ├── def _make_rich_tree(
    │   │       label,
    │   │       style: str = TEXT_COLOR,
    │   │       guide_style: str = LINK_COLOR,
    │   │       highlight: bool = True,
    │   │   ) -> Tree
    │   ├── def into_rich_tree(
    │   │       *,
    │   │       root: Optional[TreePlus] = None,
    │   │       timeout=INTO_RICH_TREE_TIMEOUT_SECONDS,
    │   │   ) -> Tree
    │   ├── def _into_rich_tree(*, root: Optional[TreePlus] = None) -> Tree
    │   ├── def is_url(x: str) -> bool
    │   ├── @lru_cache
    │   │   def categorize(
    │   │       x: Union[Path, Tuple, str],
    │   │       check_strs_globs: bool = True,
    │   │       check_strs_paths: bool = True,
    │   │       check_strs_urls: bool = True,
    │   │       raise_if_component: bool = True,
    │   │   ) -> Category
    │   ├── def from_seed(
    │   │       maybe_seed: Optional[Union] = None,
    │   │       *,
    │   │       maybe_ignore: Optional[Tuple] = DEFAULT_IGNORE,
    │   │       maybe_globs: Optional[Tuple] = None,
    │   │       tokenizer_name: TokenizerName = TokenizerName.WC,
    │   │       syntax_highlighting: bool = False,
    │   │       override_ignore: bool = False,
    │   │       concise: bool = False,
    │   │   ) -> TreePlus
    │   ├── def from_seeds(
    │   │       maybe_seeds: Optional[Tuple[Union, ...]] = None,
    │   │       *,
    │   │       maybe_ignore: Optional[Tuple] = DEFAULT_IGNORE,
    │   │       maybe_globs: Optional[Tuple] = None,
    │   │       syntax_highlighting: bool = False,
    │   │       override_ignore: bool = False,
    │   │       concise: bool = False,
    │   │       tokenizer_name: TokenizerName = TokenizerName.WC,
    │   │       regex_timeout: Optional = None,
    │   │   ) -> TreePlus
    │   ├── def _reduce_forest(
    │   │       *,
    │   │       forest: Tuple[TreePlus, ...],
    │   │   ) -> TreePlus
    │   ├── def _map_seeds(
    │   │       *,
    │   │       seeds: Optional[Tuple[Union, ...]] = None,
    │   │       maybe_ignore: Optional[Tuple] = DEFAULT_IGNORE,
    │   │       maybe_globs: Optional[Tuple] = None,
    │   │       syntax_highlighting: bool = False,
    │   │       tokenizer_name: TokenizerName = TokenizerName.WC,
    │   │       regex_timeout: Optional = None,
    │   │       concise: bool = False,
    │   │   ) -> Tuple[TreePlus, ...]
    │   ├── def _from_seed(
    │   │       *,
    │   │       seed_path: Optional[Union[Path, str]] = None,
    │   │       maybe_ignore: Optional[Tuple] = DEFAULT_IGNORE,
    │   │       maybe_globs: Optional[AmortizedGlobs] = None,
    │   │       syntax_highlighting: bool = False,
    │   │       tokenizer_name: TokenizerName = TokenizerName.WC,
    │   │       regex_timeout: Optional = None,
    │   │       concise: bool = False,
    │   │       is_url: bool = False,
    │   │   ) -> TreePlus
    │   ├── def _add_subtree(
    │   │       *,
    │   │       root: TreePlus,
    │   │       subtree: TreePlus,
    │   │   )
    │   ├── def _from_glob(
    │   │       *,
    │   │       pattern: str,
    │   │       maybe_ignore: Optional[Tuple] = DEFAULT_IGNORE,
    │   │       maybe_globs: Optional[AmortizedGlobs] = None,
    │   │       syntax_highlighting: bool = False,
    │   │       tokenizer_name: TokenizerName = TokenizerName.WC,
    │   │       regex_timeout: Optional = None,
    │   │       concise: bool = False,
    │   │   ) -> TreePlus
    │   ├── def _from_folder(
    │   │       *,
    │   │       folder_path: Path,
    │   │       maybe_ignore: Optional[Tuple] = DEFAULT_IGNORE,
    │   │       maybe_globs: Optional[AmortizedGlobs] = None,
    │   │       syntax_highlighting: bool = False,
    │   │       tokenizer_name: TokenizerName = TokenizerName.WC,
    │   │       regex_timeout: Optional = None,
    │   │       concise: bool = False,
    │   │   ) -> TreePlus
    │   ├── def _from_file(
    │   │       *,
    │   │       file_path: Path,
    │   │       syntax_highlighting: bool = False,
    │   │       tokenizer_name: TokenizerName = TokenizerName.WC,
    │   │       max_tokens: int = MAX_TOKENS,
    │   │       regex_timeout: Optional = None,
    │   │       concise: bool = False,
    │   │   ) -> TreePlus
    │   ├── def _from_url(
    │   │       *,
    │   │       url: str,
    │   │       syntax_highlighting: bool = False,
    │   │       concise: bool = False,
    │   │   ) -> TreePlus
    │   ├── def base_url(url: str, with_path: bool = False) -> str
    │   ├── def _from_html_text(contents: str, maybe_url_base: Optional = None) -> TreePlus
    │   ├── def empty_tag_tree(n: str = "?")
    │   ├── def union_from_element(elem: PageElement) -> Union[Tag, NavigableString]
    │   ├── def node_index_str_from_tuple(
    │   │       node_index: Tuple,
    │   │       prefix: str = "(",
    │   │       suffix: str = ")",
    │   │       number_color: str = CYBERCYAN,
    │   │       dot_color: str = GOLD,
    │   │   ) -> str
    │   ├── def _from_soup(
    │   │       tag: Union[Tag, NavigableString],
    │   │       tree: Optional[TreePlus] = None,
    │   │       node_index: Tuple = (),
    │   │       maybe_url_base: Optional = None,
    │   │       hrefs: Optional[Dict] = None,
    │   │   ) -> Optional[TreePlus]
    │   ├── def from_hacker_news_articles(
    │   │       articles: Articles,
    │   │       depth: int = 0,
    │   │       max_depth: int = -1,
    │   │       title: Union = "Hacker News Front Page",
    │   │       parent_num: Tuple = (),
    │   │   ) -> TreePlus
    │   ├── def format_link(
    │   │       url: str,
    │   │       text: str,
    │   │       link_color: str = LINK_COLOR,
    │   │   ) -> str
    │   ├── def process_hacker_news_item(
    │   │       item: dict,
    │   │       kids: Articles,
    │   │       depth: int,
    │   │       max_depth: int,
    │   │       parent_num: Tuple,
    │   │       parser: Union[Literal["lxml"], Literal["html.parser"]] = "html.parser",
    │   │       link_color: str = LINK_COLOR,
    │   │   ) -> Optional[TreePlus]
    │   ├── def rich_links_from_soup(
    │   │       item_soup: BeautifulSoup,
    │   │       recursive: bool = True,
    │   │   ) -> List
    │   ├── def ordered_list_from(ordered_list: Iterable) -> List
    │   ├── def _get_lexer(file_path: Path) -> str
    │   └── def _syntax_highlight(
    │           *,
    │           file_path: Path,
    │           components: List,
    │       ) -> Union[List[Syntax], List]
    ├── 📄 ignore.py (2,342 tokens, 332 lines)
    │   ├── TODO: incorporate gitignore
    │   ├── def _is_all_str(x: Any) -> bool
    │   ├── def can_parse(x) -> bool
    │   ├── @lru_cache
    │   │   def parse_ignore(
    │   │       maybe_ignore_tuple: Optional[Tuple] = None, override: bool = False
    │   │   ) -> Optional[Tuple]
    │   ├── @lru_cache
    │   │   def is_glob(x: str) -> bool
    │   ├── @lru_cache
    │   │   def parse_globs(
    │   │       maybe_globs_tuple: Optional[Tuple] = None,
    │   │   ) -> Optional[Tuple]
    │   ├── @dataclass(frozen=True)
    │   │   class AmortizedGlobs
    │   ├──     paths: Tuple[Path, ...]
    │   ├──     globs: Tuple
    │   ├──     matches: FrozenSet[Path]
    │   ├── def amortize_globs(
    │   │       paths: Tuple[Path, ...],
    │   │       globs: Tuple,
    │   │   ) -> Optional[AmortizedGlobs]
    │   └── @lru_cache(maxsize=None)
    │       def should_ignore(
    │           path: Path,
    │           ignore: Optional[Tuple] = DEFAULT_IGNORE,
    │           globs: Optional[AmortizedGlobs] = None,
    │       ) -> bool
    ├── 📄 isabelle_symbols.py (2,146 tokens, 462 lines)
    │   ├── @lru_cache
    │   │   def _replace_symbol(match: re.Match) -> str
    │   └── def replace_isabelle_symbols(content: str) -> str
    ├── 📄 parse_file.py (24,623 tokens, 2,713 lines)
    │   ├── BUG: HTML tree doesn't look awesome (yet)
    │   ├── TODO: Fix HTML in TreePlus (How?)
    │   ├── BUG: this repeatedly finds tags, need to avoid repeating ourselves
    │   ├── TODO: gather requirements for C namespace visualization
    │   ├── TODO: update parse_objective_c to avoid fixed unrolling
    │   ├── TODO: update parse_ocaml to avoid forced unrolling
    │   ├── TODO: fix parse_apl to avoid forced unrolling
    │   ├── TODO: fix parse_perl to avoid forced unrolling
    │   ├── 
    │   │   DEFAULT_REGEX_TIMEOUT = 0
    │   ├── 
    │   │   BINARY_CHECK_SIZE = 1024
    │   ├── def head(n: int, content: str) -> str
    │   ├── @lru_cache(maxsize=None)
    │   │   def read_file(
    │   │       file_path: str,
    │   │       raise_exceptions: bool = False,
    │   │       n_lines: Optional = None,
    │   │   ) -> str
    │   ├── def parse_file(
    │   │       file_path: Union,
    │   │       content: Optional = None,
    │   │       regex_timeout: Optional = None,
    │   │   ) -> List
    │   ├── def extract_groups(match: regex.Match, named_only: bool = False) -> dict
    │   ├── def parse_html(content: str) -> List
    │   ├── 
    │   │   
    │   │   DENY_HTML = "
"
    │   ├── def parse_jsonl(content: str) -> List
    │   ├── def process_tag(tag, components) -> Optional
    │   ├── def components_from_html(content: str) -> List
    │   ├── def prettify_tr(component: str) -> str
    │   ├── def assemble_tensorflow_flag(
    │   │       flag_type: str, flag: str, description: Optional[List] = None
    │   │   ) -> str
    │   ├── def parse_tensorflow_flags(
    │   │       content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT
    │   │   ) -> List
    │   ├── def parse_rst(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_c(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── @lru_cache
    │   │   def is_binary_string(data: bytes) -> bool
    │   ├── @lru_cache
    │   │   def is_binary(file_path: str) -> bool
    │   ├── def clean_isabelle_text(content: str) -> str
    │   ├── def parse_isabelle(
    │   │       content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT
    │   │   ) -> List
    │   ├── def parse_fortran(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def remove_c_comments(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> str
    │   ├── def parse_ts(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def remove_py_comments(
    │   │       input_string: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT
    │   │   ) -> str
    │   ├── def remove_docstrings(source, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> str
    │   ├── def parse_py(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_rb(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_fsharp(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_tcl(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_erl(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_rs(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_csv(content: str, max_leaves=11) -> List
    │   ├── def parse_mathematica(
    │   │       content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT
    │   │   ) -> List
    │   ├── def parse_r(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_zig(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_hs(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_lisp(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_capnp(content: str) -> List
    │   ├── def parse_grpc(content: str) -> List
    │   ├── def parse_openrpc_json(content: str) -> List
    │   ├── def parse_json_rpc(content: str) -> List
    │   ├── def parse_graphql(content: str) -> List
    │   ├── def format_dependency(name, details)
    │   ├── def parse_cargo_toml(content: str) -> List
    │   ├── def parse_pyproject_toml(content: str) -> List
    │   ├── def parse_lean(
    │   │       lean_content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT
    │   │   ) -> List
    │   ├── def parse_cs(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_tex(tex_content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_go(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_swift(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_bash(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_d_dot_ts(
    │   │       content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT
    │   │   ) -> List
    │   ├── def parse_angular_app_module(
    │   │       content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT
    │   │   ) -> List
    │   ├── def parse_angular_routes(
    │   │       content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT
    │   │   ) -> List
    │   ├── def parse_angular_spec(
    │   │       content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT
    │   │   ) -> List
    │   ├── def parse_environment_ts(
    │   │       content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT
    │   │   ) -> List
    │   ├── def parse_dot_env(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_requirements_txt(content: str) -> List
    │   ├── def parse_json_schema(content: str) -> List
    │   ├── def parse_package_json(content: str) -> List
    │   ├── def parse_makefile(content: str) -> List
    │   ├── def parse_sql(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def is_openapi_yml(ymls: Tuple) -> bool
    │   ├── def is_k8s_yml(ymls: Tuple) -> bool
    │   ├── def is_ansible_yml(ymls: Tuple) -> bool
    │   ├── def is_github_yml(ymls: Tuple) -> bool
    │   ├── def parse_github_yml(ymls: Tuple) -> List
    │   ├── def parse_k8s(ymls: Tuple) -> List
    │   ├── def parse_ansible(ymls: Tuple) -> List
    │   ├── def parse_openapi_yml(ymls: Tuple) -> List
    │   ├── def parse_yml(content: str) -> List
    │   ├── def parse_db(db_path: str) -> List
    │   ├── def dedent_components(
    │   │       components: List, *, timeout: float = DEFAULT_REGEX_TIMEOUT
    │   │   ) -> List
    │   ├── def parse_cbl(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_java(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_jl(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_kt(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_lua(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_objective_c(
    │   │       content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT
    │   │   ) -> List
    │   ├── def parse_ocaml(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_apl(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_perl(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_php(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_ps1(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_matlab(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_scala(content: str, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_tf(content: str, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_md(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   ├── def parse_txt(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    │   └── def parse_markers(content: str, *, timeout: float = DEFAULT_REGEX_TIMEOUT) -> List
    ├── 📁 scripts (1 folder, 1 file) 
    │   └── 📄 alias_tree_plus.sh (241 tokens, 30 lines)
    │       ├── add_alias()
    │       └── create_conda_env()
    ├── 📄 version.py (12 tokens, 1 line)
    │   └── __version__ = "1.0.61"
    └── 📄 web.py (2,409 tokens, 321 lines)
        ├── TODO: re-enable tree plus web actions
        ├── NOTE: no point in the answers since there's no content
        ├── 
        │   N_GOOGLE_RESULTS = 2
        ├── def create_url(kind: Action, query: str) -> str
        ├── def create_link(kind: Action, query: str) -> str
        ├── def create_wikipedia_url(subterm: str) -> str
        ├── def create_wikipedia_search_url(subterm: str) -> str
        ├── def create_google_search_url(subterm: str) -> str
        ├── def create_stack_overflow_search_url(subterm: str) -> str
        ├── def create_wikipedia_search_link(
        │       subterm: str,
        │       prefix: str = "",
        │       suffix: str = "",
        │       link_style: str = LINK_STYLE,
        │   ) -> str
        ├── def create_google_search_link(
        │       subterm: str,
        │       prefix: str = "",
        │       suffix: str = "",
        │       link_style: str = LINK_STYLE,
        │   ) -> str
        ├── def create_stack_overflow_search_link(
        │       subterm: str,
        │       prefix: str = "",
        │       suffix: str = "",
        │       link_style: str = LINK_STYLE,
        │   ) -> str
        ├── def search_stack_overflow(
        │       *,
        │       titled: Optional = None,
        │       tagged: Optional = None,
        │       answered: Optional = True,
        │   ) -> Tuple[Tuple[dict, Tuple], ...]
        ├── def table_from_df(
        │       results: Tuple[Dict, ...],
        │       title: Optional = None,
        │       show_lines: bool = True,
        │   ) -> Table
        ├── def articles_from_hacker_news(
        │       max_depth: int = 2,
        │       current_depth: int = 0,
        │       n_articles: int = 3,
        │       sleep_time: float = 0.00,
        │   ) -> Articles
        ├── def hacker_news_article_from_id(
        │       article_id: int,
        │       depth: int,
        │       current_depth: int,
        │       n: int,
        │       sleep_time: float,
        │   ) -> Tuple[HList, Tuple[HList, ...]]
        └── def fetch_kids(
                article_id: int,
                kid_ids: List,
                depth: int,
                current_depth: int,
                n: int,
                sleep_time: float,
            ) -> Tuple[HList, ...]

tree_plus v(1.0.61) ignore=('tests',) globs=() syntax=False paths=()
7 folder(s), 27 file(s), 13,236 line(s), 116,327 token(s) in 0.30 second(s).

```
<!-- t1-end -->
- [x] Demonstrate Parsed Checkboxes


## Start Quick!

### Prerequisites

Ensure you have the following tools installed:

- [Miniconda3 (conda)](https://docs.conda.io/en/latest/miniconda.html)
- [GitHub CLI (gh)](https://cli.github.com/manual/installation)

### Install Tree Plus

#### PyPI

```bash
pip install tree_plus
```

#### Local Hackable Install

1. Clone the repository and cd into the tree_plus directory
```bash
gh repo clone https://github.com/bionicles/tree_plus.git && cd tree_plus
```

2. Install tree_plus using Makefile
```bash
make cli
```

Alternatively, if `make` is not installed, use `pip`:
```bash
pip install -e .[dev]
```

Now, you can use the `tree_plus` command.

3. Add slick aliases like `tp` to your RC file

Typical RC_FILE values `~/.bash_profile`, `~/.bashrc`, or `~/.zshrc`

Use our **idempotent aliasing script**: [alias_tree_plus.sh](https://github.com/bionicles/tree_plus/blob/main/tree_plus_src/scripts/alias_tree_plus.sh)
```bash
RC_FILE=~/.bash_profile source alias_tree_plus.sh
```
```bash
RC_FILE=~/.bashrc source alias_tree_plus.sh
```
```zsh
RC_FILE=~/.zshrc source alias_tree_plus.sh
```

### Alias Usage

Run `tree_plus` with a shorter alias:

```sh
tp
```

Reinstall `tree_plus` quickly:

```sh
tpcli
```

`cd` (change directory) to the `TREE_PLUS_PATH`:

```sh
cdtp
```

Run "debug mode" (test runner)

Ensure your rig has a global install of [nodemon](https://www.npmjs.com/package/nodemon):
```sh
npm install -g nodemon
```

Watch for changes to auto re-test `tree_plus`:
```sh
make debug
```

## Library Usage:

Here's how to import the library
```py
import tree_plus_src as tp
```

Check out the **tree_plus_programs** folder for a library usage example.
(More to come!) 

To run the library usage example (make sure you've installed `tree_plus`!)
```sh
make library_demo
```

## Moar Languages

### Currently Tested Languages:
<!-- t6-start -->
```sh
tree_plus -c -i group_todo tests/more_languages
📁 more_languages (9 folders, 84 files) 
├── 📁 group1 (1 folder, 11 files) 
│   ├── 📄 addamt.cobol (441 tokens, 40 lines)
│   ├── 📄 CUSTOMER-INVOICE.CBL (412 tokens, 60 lines)
│   ├── 📄 JavaTest.java (578 tokens, 86 lines)
│   ├── 📄 JuliaTest.jl (381 tokens, 63 lines)
│   ├── 📄 KotlinTest.kt (974 tokens, 171 lines)
│   ├── 📄 lesson.cbl (635 tokens, 78 lines)
│   ├── 📄 LuaTest.lua (83 tokens, 16 lines)
│   ├── 📄 ObjectiveCTest.m (62 tokens, 16 lines)
│   ├── 📄 OcamlTest.ml (49 tokens, 12 lines)
│   ├── 📄 test.js (757 tokens, 154 lines)
│   └── 📄 test.ts (832 tokens, 165 lines)
├── 📁 group2 (1 folder, 8 files) 
│   ├── 📄 apl_test.apl (28 tokens, 5 lines)
│   ├── 📄 c_test.c (837 tokens, 142 lines)
│   ├── 📄 go_test.go (179 tokens, 46 lines)
│   ├── 📄 PerlTest.pl (63 tokens, 20 lines)
│   ├── 📄 PhpTest.php (70 tokens, 19 lines)
│   ├── 📄 PowershellTest.ps1 (459 tokens, 89 lines)
│   ├── 📄 ScalaTest.scala (171 tokens, 40 lines)
│   └── 📄 test.csv (0 tokens, 0 lines)
├── 📁 group3 (1 folder, 16 files) 
│   ├── 📄 bash_test.sh (127 tokens, 22 lines)
│   ├── 📄 cpp_test.cpp (1,670 tokens, 259 lines)
│   ├── 📄 csharp_test.cs (957 tokens, 146 lines)
│   ├── 📄 hallucination.tex (1,633 tokens, 126 lines)
│   ├── 📄 ruby_test.rb (138 tokens, 37 lines)
│   ├── 📄 swift_test.swift (469 tokens, 110 lines)
│   ├── 📄 test.lean (289 tokens, 42 lines)
│   ├── 📄 test.capnp (117 tokens, 30 lines)
│   ├── 📄 test.graphql (66 tokens, 21 lines)
│   ├── 📄 test.proto (142 tokens, 34 lines)
│   ├── 📄 test.sqlite (0 tokens, 0 lines)
│   ├── 📄 test_Cargo.toml (119 tokens, 18 lines)
│   ├── 📄 test_json_rpc_2_0.json (26 tokens, 6 lines)
│   ├── 📄 test_openapi.yaml (753 tokens, 92 lines)
│   ├── 📄 test_openrpc.json (225 tokens, 44 lines)
│   └── 📄 test_pyproject.toml (304 tokens, 39 lines)
├── 📁 group4 (1 folder, 10 files) 
│   ├── 📄 erl_test.erl (480 tokens, 68 lines)
│   ├── 📄 haskell_test.hs (414 tokens, 41 lines)
│   ├── 📄 mathematica_test.nb (133 tokens, 21 lines)
│   ├── 📄 matlab_test.m (48 tokens, 12 lines)
│   ├── 📄 RTest.R (367 tokens, 46 lines)
│   ├── 📄 rust_test.rs (974 tokens, 188 lines)
│   ├── 📄 test.zig (397 tokens, 60 lines)
│   ├── 📄 test_fsharp.fs (92 tokens, 27 lines)
│   ├── 📄 test_tcl_tk.tcl (54 tokens, 16 lines)
│   └── 📄 tf_test.tf (202 tokens, 38 lines)
├── 📁 group5 (1 folder, 19 files) 
│   ├── 📄 ansible_test.yml (55 tokens, 14 lines)
│   ├── 📄 app-routing.module.ts (287 tokens, 28 lines)
│   ├── 📄 app.component.spec.ts (410 tokens, 47 lines)
│   ├── 📄 app.component.ts (271 tokens, 45 lines)
│   ├── 📄 app.module.ts (374 tokens, 43 lines)
│   ├── 📄 checkbox_test.md (176 tokens, 21 lines)
│   ├── 📄 checkbox_test.txt (257 tokens, 33 lines)
│   ├── 📄 environment.test.ts (197 tokens, 19 lines)
│   ├── 📄 hello_world.pyi (22 tokens, 3 lines)
│   ├── 📄 k8s_test.yaml (140 tokens, 37 lines)
│   ├── 📄 Makefile (714 tokens, 84 lines)
│   ├── 📄 requirements_test.txt (29 tokens, 10 lines)
│   ├── 📄 rust_todo_test.rs (92 tokens, 26 lines)
│   ├── 📄 sql_test.sql (270 tokens, 51 lines)
│   ├── 📄 standard-app-routing.module.ts (100 tokens, 16 lines)
│   ├── 📄 test.env (190 tokens, 25 lines)
│   ├── 📄 testJsonSchema.json (421 tokens, 48 lines)
│   ├── 📄 testPackage.json (349 tokens, 43 lines)
│   └── 📄 tickets.component.ts (7,160 tokens, 903 lines)
├── 📁 group6 (1 folder, 13 files) 
│   ├── 📄 catastrophic.c (5,339 tokens, 754 lines)
│   ├── 📄 cpp_examples_impl.cc (60 tokens, 10 lines)
│   ├── 📄 cpp_examples_impl.cu (37 tokens, 10 lines)
│   ├── 📄 cpp_examples_impl.h (22 tokens, 6 lines)
│   ├── 📄 fractal.thy (1,712 tokens, 147 lines)
│   ├── 📄 Microsoft.PowerShell_profile.ps1 (3,346 tokens, 497 lines)
│   ├── 📄 python_complex_class.py (10 tokens, 2 lines)
│   ├── 📄 ramda__cloneRegExp.js (173 tokens, 9 lines)
│   ├── 📄 ramda_prop.js (646 tokens, 85 lines)
│   ├── 📄 tensorflow_flags.h (7,628 tokens, 668 lines)
│   ├── 📄 test.f (181 tokens, 30 lines)
│   ├── 📄 torch.rst (60 tokens, 8 lines)
│   └── 📄 yc.html (9,063 tokens, 169 lines)
├── 📁 group7 (1 folder, 3 files) 
│   ├── 📄 absurdly_huge.jsonl (8,347 tokens, 126 lines)
│   ├── 📄 angular_crud.ts (1,192 tokens, 148 lines)
│   └── 📄 structure.py (400 tokens, 92 lines)
└── 📁 group_lisp (1 folder, 4 files) 
    ├── 📄 clojure_test.clj (682 tokens, 85 lines)
    ├── 📄 LispTest.lisp (25 tokens, 6 lines)
    ├── 📄 racket_struct.rkt (14 tokens, 1 line)
    └── 📄 test_scheme.scm (360 tokens, 44 lines)

tree_plus v(1.0.61) ignore=('group_todo',) globs=() concise=True paths=('tests/more_languages',)
9 folder(s), 84 file(s), 7,158 line(s), 68,018 token(s) in 0.15 second(s).

```
<!-- t6-end -->
### Without the -c "Concise" Flag:
<!-- t2-start -->
```sh
tree_plus -i group_todo tests/more_languages
📁 more_languages (9 folders, 84 files) 
├── 📁 group1 (1 folder, 11 files) 
│   ├── 📄 addamt.cobol (441 tokens, 40 lines)
│   │   ├── IDENTIFICATION DIVISION.
│   │   ├── PROGRAM-ID.
│   │   │              ADDAMT.
│   │   ├── DATA DIVISION.
│   │   ├── WORKING-STORAGE SECTION.
│   │   ├── 01  KEYED-INPUT.
│   │   ├──     05  CUST-NO-IN.
│   │   ├──     05  AMT1-IN.
│   │   ├──     05  AMT2-IN.
│   │   ├──     05  AMT3-IN.
│   │   ├── 01  DISPLAYED-OUTPUT.
│   │   ├──     05  CUST-NO-OUT.
│   │   ├──     05  TOTAL-OUT.
│   │   ├── 01  MORE-DATA.
│   │   ├── PROCEDURE DIVISION.
│   │   └── 100-MAIN.
│   ├── 📄 CUSTOMER-INVOICE.CBL (412 tokens, 60 lines)
│   │   ├── IDENTIFICATION DIVISION.
│   │   ├── PROGRAM-ID. CUSTOMER-INVOICE.
│   │   ├── AUTHOR. JANE DOE.
│   │   ├── DATE. 2023-12-30.
│   │   ├──   DATE-COMPILED. 06/30/10.
│   │   ├──     DATE-WRITTEN. 12/34/56.
│   │   ├── ENVIRONMENT DIVISION.
│   │   ├── INPUT-OUTPUT SECTION.
│   │   ├── FILE-CONTROL.
│   │   ├──     SELECT CUSTOMER-FILE.
│   │   ├──     SELECT INVOICE-FILE.
│   │   ├──     SELECT REPORT-FILE.
│   │   ├── DATA DIVISION.
│   │   ├── FILE SECTION.
│   │   ├── FD CUSTOMER-FILE.
│   │   ├── 01 CUSTOMER-RECORD.
│   │   ├──    05 CUSTOMER-ID.
│   │   ├──    05 CUSTOMER-NAME.
│   │   ├──    05 CUSTOMER-BALANCE.
│   │   ├── FD INVOICE-FILE.
│   │   ├── 01 INVOICE-RECORD.
│   │   ├──    05 INVOICE-ID.
│   │   ├──    05 CUSTOMER-ID.
│   │   ├──    05 INVOICE-AMOUNT.
│   │   ├── FD REPORT-FILE.
│   │   ├── 01 REPORT-RECORD.
│   │   ├── WORKING-STORAGE SECTION.
│   │   ├── 01 WS-CUSTOMER-FOUND.
│   │   ├── 01 WS-END-OF-FILE.
│   │   ├── 01 WS-TOTAL-BALANCE.
│   │   ├── PROCEDURE DIVISION.
│   │   ├── 0000-MAIN-ROUTINE.
│   │   ├── 1000-PROCESS-RECORDS.
│   │   ├── 1100-UPDATE-CUSTOMER-BALANCE.
│   │   └── END PROGRAM CUSTOMER-INVOICE.
│   ├── 📄 JavaTest.java (578 tokens, 86 lines)
│   │   ├── abstract class LivingBeing
│   │   ├──     abstract void breathe()
│   │   ├── interface Communicator
│   │   ├──     String communicate()
│   │   ├── @Log
│   │   ├── @Getter
│   │   ├── @Setter
│   │   ├── class Person extends LivingBeing implements Communicator
│   │   ├──     Person(String name, int age)
│   │   ├──     @Override
│   │   ├──     void breathe()
│   │   ├──     @Override
│   │   ├──     public String communicate()
│   │   ├──     void greet()
│   │   ├──     String personalizedGreeting(String greeting, Optional<Boolean> includeAge)
│   │   ├── @Singleton
│   │   ├── @RestController
│   │   ├── @SpringBootApplication
│   │   ├── public class Example
│   │   ├──     @Inject
│   │   ├──     public Example(Person person)
│   │   ├──     @RequestMapping("/greet")
│   │   ├──     String home(@RequestParam(value = "name", defaultValue = "World") String name,
│   │   │                   @RequestParam(value = "age", defaultValue = "30") int age)
│   │   └──     public static void main(String[] args)
│   ├── 📄 JuliaTest.jl (381 tokens, 63 lines)
│   │   ├── module JuliaTest_EdgeCase
│   │   ├── struct Location
│   │   │       name::String 
│   │   │       lat::Float32
│   │   │       lon::Float32
│   │   │   end
│   │   ├── mutable struct mPerson
│   │   │       name::String
│   │   │       age::Int
│   │   │   end
│   │   ├── Base.@kwdef mutable struct Param
│   │   │       Δt::Float64 = 0.1
│   │   │       n::Int64
│   │   │       m::Int64
│   │   │   end
│   │   ├──     sic(x,y)
│   │   ├── welcome(l::Location)
│   │   ├── ∑(α, Ω)
│   │   ├── function noob()
│   │   │   end
│   │   ├── function ye_olde(hello::String, world::Location)
│   │   │   end
│   │   ├── function multiline_greet(
│   │   │           p::mPerson, 
│   │   │           greeting::String
│   │   │       )
│   │   │   end
│   │   ├── function julia_is_awesome(prob::DiffEqBase.AbstractDAEProblem{uType, duType, tType,
│   │   │           isinplace};
│   │   │       kwargs...) where {uType, duType, tType, isinplace}
│   │   │   end
│   │   └── end
│   ├── 📄 KotlinTest.kt (974 tokens, 171 lines)
│   │   ├── data class Person(val name: String)
│   │   ├── fun greet(person: Person)
│   │   ├── fun <T> processItems(items: List<T>, processor: (T) -> Unit)
│   │   ├── interface Source<out T>
│   │   ├──     fun nextT(): T
│   │   ├── fun MutableList<Int>.swap(index1: Int, index2: Int)
│   │   ├── fun Any?.toString(): String
│   │   ├── tailrec fun findFixPoint(x: Double = 1.0): Double
│   │   ├── class GenericRepository<T>
│   │   ├──     fun getItem(id: Int): T?
│   │   ├── sealed interface Error
│   │   ├── sealed class IOError(): Error
│   │   ├── object Runner
│   │   ├──     inline fun <reified S: SomeClass<T>, T> run() : T
│   │   ├── infix fun Int.shl(x: Int): Int
│   │   ├── class MyStringCollection
│   │   ├──     infix fun add(s: String)
│   │   ├──     fun build()
│   │   ├── open class Base(p: Int)
│   │   ├── class Derived(p: Int) : Base(p)
│   │   ├── open class Shape
│   │   ├──     open fun draw()
│   │   ├──     fun fill()
│   │   ├──     open fun edge(case: Int)
│   │   ├── interface Thingy
│   │   ├──     fun edge()
│   │   ├── class Circle() : Shape(), Thingy
│   │   ├──     override fun draw()
│   │   ├──     final override fun edge(case: Int)
│   │   ├── interface Base
│   │   ├──     fun print()
│   │   ├── class BaseImpl(val x: Int) : Base
│   │   ├──     override fun print()
│   │   ├── internal class Derived(b: Base) : Base by b
│   │   ├── class Person constructor(firstName: String)
│   │   ├── class People(
│   │   │       firstNames: Array<String>,
│   │   │       ages: Array<Int>(42),
│   │   │   )
│   │   ├──     fun edgeCases(): Boolean
│   │   ├── class Alien public @Inject constructor(
│   │   │       val firstName: String,
│   │   │       val lastName: String,
│   │   │       var age: Int,
│   │   │       val pets: MutableList<Pet> = mutableListOf(),
│   │   │   )
│   │   ├──     fun objectOriented(): String
│   │   ├──  enum class IntArithmetics : BinaryOperator<Int>, IntBinaryOperator
│   │   ├──     PLUS {
│   │   │           override fun apply(t: Int, u: Int): Int
│   │   ├──     TIMES {
│   │   │           override fun apply(t: Int, u: Int): Int
│   │   ├──     override fun applyAsInt(t: Int, u: Int)
│   │   ├── fun reformat(
│   │   │       str: String,
│   │   │       normalizeCase: Boolean = true,
│   │   │       upperCaseFirstLetter: Boolean = true,
│   │   │       divideByCamelHumps: Boolean = false,
│   │   │       wordSeparator: Char = ' ',
│   │   │   )
│   │   ├── operator fun Point.unaryMinus()
│   │   ├── abstract class Polygon
│   │   └──     abstract fun draw()
│   ├── 📄 lesson.cbl (635 tokens, 78 lines)
│   │   ├── IDENTIFICATION DIVISION.
│   │   ├── PROGRAM-ID.    CBL0002.
│   │   ├── AUTHOR.        Otto B. Fun.
│   │   ├── ENVIRONMENT DIVISION.
│   │   ├── INPUT-OUTPUT SECTION.
│   │   ├── FILE-CONTROL.
│   │   ├──     SELECT PRINT-LINE.
│   │   ├──     SELECT ACCT-REC.
│   │   ├── DATA DIVISION.
│   │   ├── FILE SECTION.
│   │   ├── FD  PRINT-LINE.
│   │   ├── 01  PRINT-REC.
│   │   ├──     05  ACCT-NO-O.
│   │   ├──     05  ACCT-LIMIT-O.
│   │   ├──     05  ACCT-BALANCE-O.
│   │   ├──     05  LAST-NAME-O.
│   │   ├──     05  FIRST-NAME-O.
│   │   ├──     05  COMMENTS-O.
│   │   ├── FD  ACCT-REC.
│   │   ├── 01  ACCT-FIELDS.
│   │   ├──     05  ACCT-NO.
│   │   ├──     05  ACCT-LIMIT.
│   │   ├──     05  ACCT-BALANCE.
│   │   ├──     05  LAST-NAME.
│   │   ├──     05  FIRST-NAME.
│   │   ├──     05  CLIENT-ADDR.
│   │   ├──         10  STREET-ADDR.
│   │   ├──         10  CITY-COUNTY.
│   │   ├──         10  USA-STATE.
│   │   ├──     05  RESERVED.
│   │   ├──     05  COMMENTS.
│   │   ├── WORKING-STORAGE SECTION.
│   │   ├── 01 FLAGS.
│   │   ├──   05 LASTREC.
│   │   ├── PROCEDURE DIVISION.
│   │   ├── OPEN-FILES.
│   │   ├── READ-NEXT-RECORD.
│   │   ├── CLOSE-STOP.
│   │   ├── READ-RECORD.
│   │   └── WRITE-RECORD.
│   ├── 📄 LuaTest.lua (83 tokens, 16 lines)
│   │   ├── function HelloWorld.new
│   │   ├── function HelloWorld.greet
│   │   └── function say_hello
│   ├── 📄 ObjectiveCTest.m (62 tokens, 16 lines)
│   │   ├── @interface HelloWorld
│   │   ├── @interface HelloWorld -> (void) sayHello
│   │   ├── @implementation HelloWorld
│   │   ├── @implementation HelloWorld -> (void) sayHello
│   │   └── void sayHelloWorld()
│   ├── 📄 OcamlTest.ml (49 tokens, 12 lines)
│   │   ├── type color
│   │   ├── class hello
│   │   ├── class hello -> method say_hello
│   │   └── let main ()
│   ├── 📄 test.js (757 tokens, 154 lines)
│   │   ├── class MyClass
│   │   ├──   myMethod()
│   │   ├──   async asyncMethod(a, b)
│   │   ├──   methodWithDefaultParameters(a = 5, b = 10)
│   │   ├──   multilineMethod(
│   │   │       c,
│   │   │       d
│   │   │     )
│   │   ├──   multilineMethodWithDefaults(
│   │   │       t = "tree",
│   │   │       p = "plus"
│   │   │     )
│   │   ├── function myFunction(param1, param2)
│   │   ├── function multilineFunction(
│   │   │     param1,
│   │   │     param2
│   │   │   )
│   │   ├── const arrowFunction = () =>
│   │   ├── const parametricArrow = (a, b) =>
│   │   ├── function ()
│   │   ├── function outerFunction(outerParam)
│   │   ├──   function innerFunction(innerParam)
│   │   ├──   innerFunction("inner")
│   │   ├── const myObject = {
│   │   ├──   myMethod: function (stuff)
│   │   ├── let myArrowObject = {
│   │   ├──   myArrow: ({
│   │   │       a,
│   │   │       b,
│   │   │       c,
│   │   │     }) =>
│   │   ├── const myAsyncArrowFunction = async () =>
│   │   ├── function functionWithRestParameters(...args)
│   │   ├── const namedFunctionExpression = function myNamedFunction()
│   │   ├── const multilineArrowFunction = (
│   │   │     a,
│   │   │     b
│   │   │   ) =>
│   │   ├── function functionReturningFunction()
│   │   ├──   return function ()
│   │   ├── function destructuringOnMultipleLines({
│   │   │     a,
│   │   │     b,
│   │   │   })
│   │   ├── const arrowFunctionWithDestructuring = ({ a, b }) =>
│   │   ├── const multilineDestructuringArrow = ({
│   │   │     a,
│   │   │     b,
│   │   │   }) =>
│   │   ├── async function asyncFunctionWithErrorHandling()
│   │   ├── class Car
│   │   ├──   constructor(brand)
│   │   ├──   present()
│   │   ├── class Model extends Car
│   │   ├──   constructor(brand, mod)
│   │   ├──     super(brand)
│   │   └──   show()
│   └── 📄 test.ts (832 tokens, 165 lines)
│       ├── type MyType
│       ├── interface MyInterface
│       ├── class TsClass
│       ├──   myMethod()
│       ├──   myMethodWithArgs(param1: string, param2: number): void
│       ├──   static myStaticMethod<T>(param: T): T
│       ├──   multilineMethod(
│       │       c: number,
│       │       d: number
│       │     ): number
│       ├──   multilineMethodWithDefaults(
│       │       t: string = "tree",
│       │       p: string = "plus"
│       │     ): string
│       ├── export class AdvancedComponent implements MyInterface
│       ├──   async myAsyncMethod(
│       │       a: string,
│       │       b: number,
│       │       c: string
│       │     ): Promise<void>
│       ├──   genericMethod<T, U>(
│       │       arg1: T,
│       │       arg2: U
│       │     ): [T, U]
│       ├── export class TicketsComponent implements MyInterface
│       ├──   async myAsyncMethod({ a, b, c }: { a: String; b: Number; c: String })
│       ├── function tsFunction()
│       ├── function tsFunctionSigned(
│       │     param1: number,
│       │     param2: number
│       │   ): void
│       ├── export default async function tsFunctionComplicated<A, B, C>({
│       │     a = 1 | 2,
│       │     b = "bob",
│       │     c = async () => "charlie",
│       │   }: {
│       │     a: number;
│       │     b: string;
│       │     c: () => Promise<string>;
│       │   }): Promise<string>
│       ├──   return("Standalone function with parameters")
│       ├── const tsArrowFunctionSigned = ({
│       │     a,
│       │     b,
│       │   }: {
│       │     a: number;
│       │     b: string;
│       │   }) =>
│       ├── export const tsComplicatedArrow = async ({
│       │     a = 1 | 2,
│       │     b = "bob",
│       │     c = async () => "charlie",
│       │   }: {
│       │     a: number;
│       │     b: string;
│       │     c: () => Promise<string>;
│       │   }): Promise<string> =>
│       ├── const arrowFunction = () =>
│       ├── const arrow = (a: String, b: Number) =>
│       ├── const asyncArrowFunction = async () =>
│       ├── const asyncArrow = async (a: String, b: Number) =>
│       ├── let weirdArrow = () =>
│       ├── const asyncPromiseArrow = async (): Promise<void> =>
│       ├── let myWeirdArrowSigned = (x: number): number =>
│       ├── class Person
│       ├──   constructor(private firstName: string, private lastName: string)
│       ├──   getFullName(): string
│       ├──   describe(): string
│       ├── class Employee extends Person
│       ├──   constructor(
│       │       firstName: string,
│       │       lastName: string,
│       │       private jobTitle: string
│       │     )
│       ├──     super(firstName, lastName)
│       ├──   describe(): string
│       ├── interface Shape
│       └── interface Square extends Shape
├── 📁 group2 (1 folder, 8 files) 
│   ├── 📄 apl_test.apl (28 tokens, 5 lines)
│   │   ├── :Namespace HelloWorld
│   │   ├── :Namespace HelloWorld -> hello ← 'Hello, World!'
│   │   └── :Namespace HelloWorld -> plus ← {⍺+⍵}
│   ├── 📄 c_test.c (837 tokens, 142 lines)
│   │   ├── struct Point
│   │   ├──     int x;
│   │   ├──     int y;
│   │   ├── struct Point getOrigin()
│   │   ├── float mul_two_floats(float x1, float x2)
│   │   ├── enum days
│   │   ├──     SUN,
│   │   ├──     MON,
│   │   ├──     TUE,
│   │   ├──     WED,
│   │   ├──     THU,
│   │   ├──     FRI,
│   │   ├──     SAT
│   │   ├── long add_two_longs(long x1, long x2)
│   │   ├── double multiplyByTwo(double num)
│   │   ├── char getFirstCharacter(char *str)
│   │   ├── void greet(Person p)
│   │   ├── typedef struct
│   │   ├──     char name[50];
│   │   ├── } Person;
│   │   ├── int main()
│   │   ├── int* getArrayStart(int arr[], int size)
│   │   ├── long complexFunctionWithMultipleArguments(
│   │   │       int param1,
│   │   │       double param2,
│   │   │       char *param3,
│   │   │       struct Point point
│   │   │   )
│   │   ├── keyPattern *ACLKeyPatternCreate(sds pattern, int flags)
│   │   ├── sds sdsCatPatternString(sds base, keyPattern *pat)
│   │   ├── static int ACLCheckChannelAgainstList(list *reference, const char *channel, int channellen, int is_pattern)
│   │   ├──     while((ln = listNext(&li)))
│   │   ├── static struct config
│   │   ├──     aeEventLoop *el;
│   │   ├──     cliConnInfo conn_info;
│   │   ├──     const char *hostsocket;
│   │   ├──     int tls;
│   │   ├──     struct cliSSLconfig sslconfig;
│   │   └── } config;
│   ├── 📄 go_test.go (179 tokens, 46 lines)
│   │   ├── type Greeting struct
│   │   ├── func (g Greeting) sayHello()
│   │   ├── func createGreeting(m string) Greeting
│   │   ├── type SomethingLong struct
│   │   ├── func (s *SomethingLong) WithAReasonableName(
│   │   │     ctx context.Context,
│   │   │     param1 string,
│   │   │     param2 int,
│   │   │     param3 mapinterface{},
│   │   │     callback func(int) error,
│   │   │   ) (resultType, error)
│   │   ├── type resultType struct
│   │   └── func main()
│   ├── 📄 PerlTest.pl (63 tokens, 20 lines)
│   │   ├── package PerlTest
│   │   ├── package PerlTest -> sub new
│   │   ├── package PerlTest -> sub hello
│   │   └── package PerlTest -> sub say_hello
│   ├── 📄 PhpTest.php (70 tokens, 19 lines)
│   │   ├── class HelloWorld
│   │   ├── class HelloWorld -> function sayHello
│   │   ├── function greet
│   │   ├── class Person
│   │   └── class Person -> function __construct
│   ├── 📄 PowershellTest.ps1 (459 tokens, 89 lines)
│   │   ├── function Say-Nothing()
│   │   ├── class Person
│   │   ├──     Person($name)
│   │   ├──     Greet()
│   │   ├──     GreetMany($times)
│   │   ├──     GreetWithDetails($greeting, $times)
│   │   ├──     GreetMultiline(
│   │   │           $greeting,
│   │   │           $times
│   │   │       )
│   │   ├──     NoReturn($times)
│   │   ├──     NoReturnNoArgs()
│   │   ├── function Say-Hello([Person]$person)
│   │   ├── function Multi-Hello([Person]$personA, [Person]$personB)
│   │   ├── function Switch-Item
│   │   ├──   param ($on)
│   │   ├── function Get-SmallFiles
│   │   ├──   param (
│   │   │         [PSDefaultValue(Help = '100')]
│   │   │         $Size = 100)
│   │   ├── function Get-User
│   │   ├──   [CmdletBinding(DefaultParameterSetName="ID")]
│   │   ├──   [OutputType("System.Int32", ParameterSetName="ID")]
│   │   ├──   [OutputType([String], ParameterSetName="Name")]
│   │   ├──   Param (
│   │   │       
│   │   │       [Int[]]
│   │   │       $UserID,
│   │   │       
│   │   │       [String[]]
│   │   │       $UserName)
│   │   ├── filter Get-ErrorLog ($Message)
│   │   └── function global:MultilineSignature(
│   │         $param1,
│   │         $param2,
│   │         [Parameter(Mandatory=$true)]
│   │         $param3
│   │       )
│   ├── 📄 ScalaTest.scala (171 tokens, 40 lines)
│   │   ├── def sumOfSquares(x: Int, y: Int): Int
│   │   ├── trait Bark
│   │   ├──   def bark: String
│   │   ├── case class Person(name: String)
│   │   ├── class GenericClass[T](
│   │   │       val data: T,
│   │   │       val count: Int
│   │   │   )
│   │   ├──   def getData: T
│   │   ├── object HelloWorld
│   │   ├──   def greet(person: Person): Unit
│   │   ├──   def main(args: Array[String]): Unit
│   │   ├── def complexFunction(
│   │   │       a: Int,
│   │   │       b: String,
│   │   │       c: Float
│   │   │   ): (Int, String) Option
│   │   └── def sumOfSquaresShort(x: Int, y: Int): Int
│   └── 📄 test.csv (0 tokens, 0 lines)
│       ├── Name
│       ├── Age
│       ├── Country
│       ├── City
│       └── Email
├── 📁 group3 (1 folder, 16 files) 
│   ├── 📄 bash_test.sh (127 tokens, 22 lines)
│   │   ├── echo_hello_world()
│   │   ├── function fun_echo_hello_world()
│   │   ├── export SECRET
│   │   ├── alias md='make debug'
│   │   ├── add_alias()
│   │   └── create_conda_env()
│   ├── 📄 cpp_test.cpp (1,670 tokens, 259 lines)
│   │   ├── class Person
│   │   ├──     std::string name;
│   │   ├── public:
│   │   ├──     Person(std::string n) : name(n)
│   │   ├──     void greet()
│   │   ├── void globalGreet()
│   │   ├── int main()
│   │   ├── void printMessage(const std::string &message)
│   │   ├── template<typename T>
│   │   │   void printVector(const std::vector<T>& vec)
│   │   ├── struct Point
│   │   ├──     int x, y;
│   │   ├──     Point(int x, int y) : x(x), y(y)
│   │   ├── class Animal
│   │   ├── public:
│   │   ├──     Animal(const std::string &name) : name(name)
│   │   ├──     virtual void speak() const
│   │   ├──     virtual ~Animal()
│   │   ├── protected:
│   │   ├──     std::string name;
│   │   ├── class Dog : public Animal
│   │   ├── public:
│   │   ├──     Dog(const std::string &name) : Animal(name)
│   │   ├──     void speak() const override
│   │   ├── class Cat : public Animal
│   │   ├── public:
│   │   ├──     Cat(const std::string &name) : Animal(name)
│   │   ├──     void speak() const override
│   │   ├── nb::bytes BuildRnnDescriptor(int input_size, int hidden_size, int num_layers,
│   │   │                                int batch_size, int max_seq_length, float dropout,
│   │   │                                bool bidirectional, bool cudnn_allow_tf32,
│   │   │              int workspace_size, int reserve_space_size)
│   │   ├── int main()
│   │   ├── enum ECarTypes
│   │   ├──   Sedan,
│   │   ├──   Hatchback,
│   │   ├──   SUV,
│   │   ├──   Wagon
│   │   ├── ECarTypes GetPreferredCarType()
│   │   ├── enum ECarTypes : uint8_t
│   │   ├──   Sedan,
│   │   ├──   Hatchback,
│   │   ├──   SUV = 254,
│   │   ├──   Hybrid
│   │   ├── enum class ECarTypes : uint8_t
│   │   ├──   Sedan,
│   │   ├──   Hatchback,
│   │   ├──   SUV = 254,
│   │   ├──   Hybrid
│   │   ├── void myFunction(string fname, int age)
│   │   ├── template <typename T> T cos(T)
│   │   ├── template <typename T> T sin(T)
│   │   ├── template <typename T> T sqrt(T)
│   │   ├── template<typename T> struct VLEN
│   │   ├── template<typename T> class arr
│   │   ├──   private:
│   │   ├──     static T *ralloc(size_t num)
│   │   ├──     static void dealloc(T *ptr)
│   │   ├──     static T *ralloc(size_t num)
│   │   ├──     static void dealloc(T *ptr)
│   │   ├──   public:
│   │   ├──     arr() : p(0), sz(0)
│   │   ├──     arr(size_t n) : p(ralloc(n)), sz(n)
│   │   ├──     arr(arr &&other)
│   │   │         : p(other.p), sz(other.sz)
│   │   ├──     ~arr()
│   │   ├──     void resize(size_t n)
│   │   ├──     T &operator[](size_t idx)
│   │   ├──     T *data()
│   │   ├──     size_t size() const
│   │   ├── class Buffer
│   │   ├──  private:
│   │   ├──   void* ptr_;
│   │   └── std::tuple<array, array, array> quantize(
│   │           const array& w,
│   │           int group_size,
│   │           int bits,
│   │           StreamOrDevice s)
│   ├── 📄 csharp_test.cs (957 tokens, 146 lines)
│   │   ├── public interface IExcelTemplate
│   │   ├──     void LoadTemplate(string templateFilePath)
│   │   ├──     void LoadData(Dictionary<string, string> data)
│   │   ├──     void ModifyCell(string cellName, string value)
│   │   ├──     void SaveToFile(string filePath)
│   │   ├── public interface IGreet
│   │   ├──     void Greet()
│   │   ├── public enum WeekDays
│   │   ├── public delegate void DisplayMessage(string message)
│   │   ├── public struct Address
│   │   ├── public static class HelperFunctions
│   │   ├──     public static void PrintMessage(string message)
│   │   ├──     public static int AddNumbers(int a, int b)
│   │   ├── namespace HelloWorldApp
│   │   ├──     class Person : IGreet
│   │   ├──         public Person(string name, int age)
│   │   ├──         public void Greet()
│   │   ├──     class HelloWorld
│   │   ├──         static void Main(string[] args)
│   │   ├── namespace TemplateToExcelServer.Template
│   │   ├──     public interface ITemplateObject
│   │   ├──         string[,] GetContent()
│   │   ├──         string[] GetContentArray()
│   │   ├──         string[] GetFormat()
│   │   ├──         int? GetFormatLength()
│   │   ├──         TemplateObject SetContent(string[,] Content)
│   │   ├──         TemplateObject SetContentArray(string[] value)
│   │   ├──         TemplateObject SetFormat(string[] Header)
│   │   ├──         TemplateObject SetNameOfReport(
│   │   │               ReadOnlyMemory<byte> ReportName,
│   │   │               int[] EdgeCase)
│   │   ├──         TemplateObject SetSheetName(ReadOnlyMemory<byte> SheetName)
│   │   ├── public class BankAccount(string accountID, string owner)
│   │   ├──     public override string ToString() =>
│   │   ├── var IncrementBy = (int source, int increment = 1) =>
│   │   ├── Func<int, int, int> add = (x, y) =>
│   │   ├── button.Click += (sender, args) =>
│   │   ├── public Func<int, int> GetMultiplier(int factor)
│   │   ├── public void Method(
│   │   │           int param1,
│   │   │           int param2,
│   │   │           int param3,
│   │   │           int param4,
│   │   │           int param5,
│   │   │           int param6,
│   │   │       )
│   │   ├── System.Net.ServicePointManager.ServerCertificateValidationCallback +=
│   │   │       (se, cert, chain, sslerror) =>
│   │   ├── class ServerCertificateValidation
│   │   ├──     public bool OnRemoteCertificateValidation(
│   │   │           object se,
│   │   │           X509Certificate cert,
│   │   │           X509Chain chain,
│   │   │           SslPolicyErrors sslerror
│   │   │       )
│   │   ├── s_downloadButton.Clicked += async (o, e) =>
│   │   ├── [HttpGet, Route("DotNetCount")]
│   │   └── static public async Task<int> GetDotNetCount(string URL)
│   ├── 📄 hallucination.tex (1,633 tokens, 126 lines)
│   │   ├── Harnessing the Master Algorithm: Strategies for AI LLMs to Mitigate Hallucinations
│   │   ├── Hallucinated Pedro Domingos et al.
│   │   ├── Christmas Eve 2023
│   │   ├── 1 Introduction
│   │   ├── 2 Representation in LLMs
│   │   ├──   2.1 Current Representational Models
│   │   ├──   2.2 Incorporating Cognitive Structures
│   │   ├──   2.3 Conceptual Diagrams of Advanced Representational Models
│   │   ├── 3 Evaluation Strategies
│   │   ├──   3.1 Existing Evaluation Metrics for LLMs
│   │   ├──   3.2 Integrating Contextual and Ethical Considerations
│   │   ├──   3.3 Case Studies: Evaluation in Practice
│   │   ├── 4 Optimization Techniques
│   │   ├──   4.1 Continuous Learning Models
│   │   ├──   4.2 Adaptive Algorithms for Real-time Adjustments
│   │   ├──   4.3 Performance Metrics Pre- and Post-Optimization
│   │   ├── 5 Interdisciplinary Insights
│   │   ├──   5.1 Cognitive Science and AI: A Symbiotic Relationship
│   │   ├──   5.2 Learning from Human Cognitive Processes
│   │   ├── 6 Challenges and Future Directions
│   │   ├──   6.1 Addressing Current Limitations
│   │   ├──   6.2 The Road Ahead: Ethical and Practical Considerations
│   │   ├── 7 Conclusion
│   │   ├──   7.1 Summarizing Key Findings
│   │   └──   7.2 The Next Steps in AI Development
│   ├── 📄 ruby_test.rb (138 tokens, 37 lines)
│   │   ├── module Greeter
│   │   ├──   def self.say_hello
│   │   ├── class HelloWorld
│   │   ├──   def say_hello
│   │   ├── class Human
│   │   ├──   def self.bar
│   │   ├──   def self.bar=(value)
│   │   ├── class Doctor < Human
│   │   └──   def brachial_plexus(
│   │             roots,
│   │             trunks,
│   │             divisions: true,
│   │             cords: [],
│   │             branches: Time.now
│   │           )
│   ├── 📄 swift_test.swift (469 tokens, 110 lines)
│   │   ├── class Person
│   │   ├──     init(name: String)
│   │   ├──     func greet()
│   │   ├──     func yEdgeCase(
│   │   │           fname: String, 
│   │   │           lname: String, 
│   │   │           age: Int,
│   │   │           address: String, 
│   │   │           phoneNumber: String
│   │   │       )
│   │   ├── func globalGreet()
│   │   ├── struct Point
│   │   ├── protocol Animal
│   │   ├──     func speak()
│   │   ├── struct Dog: Animal
│   │   ├── class Cat: Animal
│   │   ├──     init(name: String)
│   │   ├──     func speak()
│   │   ├── enum CarType
│   │   ├── func getPreferredCarType() -> CarType
│   │   ├── enum CarType: UInt8
│   │   ├── enum class CarType: UInt8
│   │   ├── func myFunction(fname: String, age: Int)
│   │   └── func myFunctionWithMultipleParameters(
│   │           fname: String, 
│   │           lname: String, 
│   │           age: Int, 
│   │           address: String, 
│   │           phoneNumber: String
│   │       )
│   ├── 📄 test.lean (289 tokens, 42 lines)
│   │   ├── # Advanced Topics in Group Theory
│   │   ├── section GroupDynamics
│   │   ├── lemma group_stability (G : Type*) [Group G] (H : Subgroup G)
│   │   ├── theorem subgroup_closure {G : Type*} [Group G] (S : Set G)
│   │   ├── axiom group_homomorphism_preservation {G H : Type*} [Group G] [Group H] (f : G → H)
│   │   ├── end GroupDynamics
│   │   ├── section ConstructiveApproach
│   │   ├── lemma finite_group_order (G : Type*) [Group G] [Fintype G]
│   │   ├── lemma complex_lemma {X Y : Type*} [SomeClass X] [AnotherClass Y]
│   │   │     (f : X → Y) (g : Y → X)
│   │   └── end ConstructiveApproach
│   ├── 📄 test.capnp (117 tokens, 30 lines)
│   │   ├── struct Employee
│   │   ├──   id @0 :Int32
│   │   ├──   name @1 :Text
│   │   ├──   role @2 :Text
│   │   ├──   skills @3 :List(Skill)
│   │   ├──   struct Skill
│   │   ├──     name @0 :Text
│   │   ├──     level @1 :Level
│   │   ├──     enum Level
│   │   ├──       beginner @0
│   │   ├──       intermediate @1
│   │   ├──       expert @2
│   │   ├──   status :union
│   │   ├──     active @4 :Void
│   │   ├──     onLeave @5 :Void
│   │   ├──     retired @6 :Void
│   │   ├── struct Company
│   │   └──   employees @0 :List(Employee)
│   ├── 📄 test.graphql (66 tokens, 21 lines)
│   │   ├── type Query
│   │   ├──     getBooks: [Book]
│   │   ├──     getAuthors: [Author]
│   │   ├── type Mutation
│   │   ├──     addBook(title: String, author: String): Book
│   │   ├──     removeBook(id: ID): Book
│   │   ├── type Book
│   │   ├──     id: ID
│   │   ├──     title: String
│   │   ├──     author: Author
│   │   ├── type Author
│   │   ├──     id: ID
│   │   ├──     name: String
│   │   └──     books: [Book]
│   ├── 📄 test.proto (142 tokens, 34 lines)
│   │   ├── syntax = "proto3"
│   │   ├── service EmployeeService
│   │   ├──     rpc GetEmployee(EmployeeId) returns (EmployeeInfo)
│   │   ├──     rpc AddEmployee(EmployeeData) returns (EmployeeInfo)
│   │   ├──     rpc UpdateEmployee(EmployeeUpdate) returns (EmployeeInfo)
│   │   ├── message EmployeeId
│   │   ├──     int32 id = 1
│   │   ├── message EmployeeInfo
│   │   ├──     int32 id = 1
│   │   ├──     string name = 2
│   │   ├──     string role = 3
│   │   ├── message EmployeeData
│   │   ├──     string name = 1
│   │   ├──     string role = 2
│   │   ├── message EmployeeUpdate
│   │   ├──     int32 id = 1
│   │   ├──     string name = 2
│   │   └──     string role = 3
│   ├── 📄 test.sqlite (0 tokens, 0 lines)
│   │   ├── students table:
│   │   ├──    id integer primary key
│   │   ├──    name text not null
│   │   ├──    age integer not null
│   │   ├── courses table:
│   │   ├──    id integer primary key
│   │   ├──    title text not null
│   │   └──    credits integer not null
│   ├── 📄 test_Cargo.toml (119 tokens, 18 lines)
│   │   ├── name: test_cargo
│   │   ├── version: 0.1.0
│   │   ├── description: A test Cargo.toml
│   │   ├── license: MIT OR Apache-2.0
│   │   ├── dependencies:
│   │   ├──   clap 4.4
│   │   └──   sqlx 0.7 (features: runtime-tokio, tls-rustls)
│   ├── 📄 test_json_rpc_2_0.json (26 tokens, 6 lines)
│   │   ├── jsonrpc: 2.0
│   │   ├── method: subtract
│   │   ├── params:
│   │   ├──     minuend: 42
│   │   ├──     subtrahend: 23
│   │   └── id: 1
│   ├── 📄 test_openapi.yaml (753 tokens, 92 lines)
│   │   ├── openapi: 3.0.1
│   │   ├──     title: TODO Plugin
│   │   ├──     description: A plugin to create and manage TODO lists using ChatGPT.
│   │   ├──     version: v1
│   │   ├── servers:
│   │   ├──     - url: PLUGIN_HOSTNAME
│   │   ├── paths:
│   │   ├──     '/todos/{username}':
│   │   ├──         GET (getTodos): Get the list of todos
│   │   ├──         POST (addTodo): Add a todo to the list
│   │   └──         DELETE (deleteTodo): Delete a todo from the list
│   ├── 📄 test_openrpc.json (225 tokens, 44 lines)
│   │   ├── openrpc: 1.2.1
│   │   ├── info:
│   │   ├──     title: Demo Petstore
│   │   ├──     version: 1.0.0
│   │   ├── methods:
│   │   ├──     listPets: List all pets
│   │   ├──         params:
│   │   ├──             - limit: integer
│   │   └──         result: pets = An array of pets
│   └── 📄 test_pyproject.toml (304 tokens, 39 lines)
│       ├── name: tree_plus
│       ├── version: 1.0.8
│       ├── description: A `tree` util enhanced with tokens, lines, and components.
│       ├── License :: OSI Approved :: Apache Software License
│       ├── License :: OSI Approved :: MIT License
│       ├── dependencies:
│       ├──     tiktoken
│       ├──     PyYAML
│       ├──     click
│       ├──     rich
│       └──     tomli
├── 📁 group4 (1 folder, 10 files) 
│   ├── 📄 erl_test.erl (480 tokens, 68 lines)
│   │   ├── -module(erl_test).
│   │   ├── -record(person).
│   │   ├── -type ra_peer_status().
│   │   ├── -type ra_membership().
│   │   ├── -opaque my_opaq_type().
│   │   ├── -type orddict(Key, Val).
│   │   ├── -type edge(
│   │   │           Cases,
│   │   │           Pwn,
│   │   │       ).
│   │   ├── -spec guarded(X) -> X when X :: tuple().
│   │   ├── -spec edge_case(
│   │   │           {integer(), any()} | 
│   │   │       ) -> processed, integer(), any()} | [{item, any()}].
│   │   ├── -spec complex_function({integer(), any()} | ) -> 
│   │   │       {processed, integer(), any()} | [{item, any()}].
│   │   ├── -spec list_manipulation() -> .
│   │   ├── -spec overload(T1, T2) -> T3
│   │   │           ; (T4, T5) -> T6.
│   │   ├── -spec multiguard({X, integer()}) -> X when X :: atom()
│   │   │           ; ([Y]) -> Y when Y :: number().
│   │   ├── -record(multiline).
│   │   └── -record(maybe_undefined).
│   ├── 📄 haskell_test.hs (414 tokens, 41 lines)
│   │   ├── data Person
│   │   ├── greet :: Person -> String
│   │   └── resolveVariables ::
│   │         forall m fragments.
│   │         (MonadError QErr m, Traversable fragments) =>
│   │         Options.BackwardsCompatibleNullInNonNullableVariables ->
│   │         [G.VariableDefinition] ->
│   │         GH.VariableValues ->
│   │         [G.Directive G.Name] ->
│   │         G.SelectionSet fragments G.Name ->
│   │         m
│   │           ( [G.Directive Variable],
│   │             G.SelectionSet fragments Variable
│   │           )
│   ├── 📄 mathematica_test.nb (133 tokens, 21 lines)
│   │   ├── person
│   │   ├── sayHello[]
│   │   └── sumList
│   ├── 📄 matlab_test.m (48 tokens, 12 lines)
│   │   ├── classdef HelloWorld -> function greet
│   │   └── function loneFun
│   ├── 📄 RTest.R (367 tokens, 46 lines)
│   │   ├── class(person)
│   │   ├── greet.Person <- function
│   │   ├── ensure_between = function
│   │   └── run_intermediate_annealing_process = function
│   ├── 📄 rust_test.rs (974 tokens, 188 lines)
│   │   ├── enum Days
│   │   ├── struct Point
│   │   ├── impl Point
│   │   ├──     fn get_origin() -> Point
│   │   ├── struct Person
│   │   ├── impl Person
│   │   ├──     fn greet(&self)
│   │   ├── fn add_two_longs(x1: i64, x2: i64) -> i64
│   │   ├── fn add_two_longs_longer(
│   │   │       x1: i64,
│   │   │       x2: i64,
│   │   │   ) -> i64
│   │   ├── fn multiply_by_two(num: f64) -> f64
│   │   ├── fn get_first_character(s: &str) -> Option<char>
│   │   ├── trait Drawable
│   │   ├──     fn draw(&self)
│   │   ├── impl Drawable for Point
│   │   ├──     fn draw(&self)
│   │   ├── fn main()
│   │   ├── pub struct VisibleStruct
│   │   ├── mod my_module
│   │   ├──     pub struct AlsoVisibleStruct<T>(T, T)
│   │   ├── macro_rules! say_hello
│   │   ├── #
│   │   │   macro_rules! hello_tree_plus
│   │   ├── pub mod lib
│   │   ├──     pub mod interfaces
│   │   ├──     mod engine
│   │   ├── pub fn flow<S1, S2, S3, S4, E, T, L>(
│   │   │       source: S1,
│   │   │       extractor: E,
│   │   │       inbox: S2,
│   │   │       transformer: T,
│   │   │       outbox: S3,
│   │   │       loader: L,
│   │   │       sink: &mut S4,
│   │   │   ) -> Result<(), Box<dyn Error>>
│   │   │   where
│   │   │       S1: Extractable,
│   │   │       S2: Extractable + Loadable,
│   │   │       S3: Extractable + Loadable,
│   │   │       S4: Loadable,
│   │   │       E: Extractor<S1, S2>,
│   │   │       T: Transformer<S2, S3>,
│   │   │       L: Loader<S3, S4>
│   │   ├── trait Container
│   │   ├──     fn items(&self) -> impl Iterator<Item = Widget>
│   │   ├── trait HttpService
│   │   ├──     async fn fetch(&self, url: Url) -> HtmlBody
│   │   ├── struct Pair<T, U>
│   │   ├── trait Transformer<T>
│   │   ├──     fn transform(&self, input: T) -> T
│   │   ├── impl<T: std::ops::Add<Output = T> + Copy> Transformer<T> for Pair<T, T>
│   │   ├──     fn transform(&self, input: T) -> T
│   │   ├── fn main()
│   │   ├── async fn handle_get(State(pool): State<PgPool>) -> Result<Html<String>, (StatusCode, String)> 
│   │   │   where
│   │   │       Bion: Cool
│   │   ├── #
│   │   │   macro_rules! unit
│   │   └──             fn insert(
│   │                       &mut self,
│   │                       key: (),
│   │                       value: $unit_dtype,
│   │                   ) -> Result<Option<$unit_dtype>, ETLError>
│   ├── 📄 test.zig (397 tokens, 60 lines)
│   │   ├── pub fn add(a: i32, b: i32) i32
│   │   ├── test "add function"
│   │   ├── const BunBuildOptions = struct
│   │   ├──     pub fn updateRuntime(this: *BunBuildOptions) anyerror!void
│   │   ├──     pub fn step(this: BunBuildOptions, b: anytype) *std.build.OptionsStep
│   │   └── pub fn sgemv(
│   │           order: Order,
│   │           trans: Trans,
│   │           m: usize,
│   │           n: usize,
│   │           alpha: f32,
│   │           a: []const f32,
│   │           lda: usize,
│   │           x: []const f32,
│   │           x_add: usize,
│   │           beta: f32,
│   │           y: []f32,
│   │           y_add: usize,
│   │       ) void
│   ├── 📄 test_fsharp.fs (92 tokens, 27 lines)
│   │   ├── module TestFSharp
│   │   ├── type Person = {
│   │   ├── let add x y =
│   │   ├── let multiply 
│   │   │       (x: int) 
│   │   │       (y: int): int =
│   │   ├── let complexFunction
│   │   │       (a: int)
│   │   │       (b: string)
│   │   │       (c: float)
│   │   │       : (int * string) option =
│   │   └── type Result<'T> =
│   ├── 📄 test_tcl_tk.tcl (54 tokens, 16 lines)
│   │   ├── proc sayHello {}
│   │   ├── proc arrg { input }
│   │   └── proc multiLine {
│   │           x,
│   │           y
│   │       }
│   └── 📄 tf_test.tf (202 tokens, 38 lines)
│       ├── provider "aws"
│       ├── resource "aws_instance" "example"
│       ├── data "aws_ami" "ubuntu"
│       ├── variable "instance_type"
│       ├── output "instance_public_ip"
│       ├── locals
│       └── module "vpc"
├── 📁 group5 (1 folder, 19 files) 
│   ├── 📄 ansible_test.yml (55 tokens, 14 lines)
│   │   ├── Install package
│   │   ├── Start service
│   │   └── Create user
│   ├── 📄 app-routing.module.ts (287 tokens, 28 lines)
│   │   ├── const routes: Routes = [
│   │   │       { path: '', redirectTo: 'login', pathMatch: 'full' },
│   │   │       { path: '*', redirectTo: 'login' },
│   │   │       { path: 'home', component: HomeComponent },
│   │   │       { path: 'login', component: LoginComponent },
│   │   │       { path: 'register', component: RegisterComponent },
│   │   │       { path: 'events', component: EventsComponent },
│   │   │       { path: 'invites', component: InvitesComponent },
│   │   │       { path: 'rewards', component: RewardsComponent },
│   │   │       { path: 'profile', component: ProfileComponent },
│   │   │   ];
│   │   └── export class AppRoutingModule
│   ├── 📄 app.component.spec.ts (410 tokens, 47 lines)
│   │   ├── describe 'AppComponent'
│   │   ├──     it should create the app
│   │   ├──     it should welcome the user
│   │   ├──     it should welcome 'Jimbo'
│   │   └──     it should request login if not logged in
│   ├── 📄 app.component.ts (271 tokens, 45 lines)
│   │   ├── export class AppComponent
│   │   ├──   constructor(
│   │   │       private http: HttpClient,
│   │   │       private loginService: LoginService,
│   │   │       private stripeService: StripeService
│   │   │     )
│   │   ├──   constructor(private loginService: LoginService)
│   │   ├──   checkSession()
│   │   ├──   async goToEvent(event_id: string)
│   │   └──   valInvitedBy(event: any, event_id: string)
│   ├── 📄 app.module.ts (374 tokens, 43 lines)
│   │   ├── @NgModule({
│   │   │       declarations: [
│   │   │           AppComponent,
│   │   │           HomeComponent,
│   │   │           LoginComponent,
│   │   │           RegisterComponent,
│   │   │           EventsComponent,
│   │   │           InvitesComponent,
│   │   │           RewardsComponent,
│   │   │           ProfileComponent
│   │   └── export class AppModule
│   ├── 📄 checkbox_test.md (176 tokens, 21 lines)
│   │   ├── # My Checkbox Test
│   │   ├── ## My No Parens Test
│   │   ├── ## My Empty href Test
│   │   ├── ## My other url Test [Q&A]
│   │   ├── ## My other other url Test [Q&A]
│   │   ├── ## My 2nd other url Test [Q&A]
│   │   ├── ## My 3rd other url Test [Q&A]
│   │   ├── - [ ] Task 1
│   │   ├──     - [ ] No Space Task 1.1
│   │   ├──     - [ ] Two Spaces Task 1.2
│   │   ├──         - [ ] Subtask 1.2.1
│   │   ├── - [ ] Task 2
│   │   ├── -  Task 3
│   │   ├──     - [ ] Subtask 3.1
│   │   ├── -  Task 6
│   │   ├──     -  Subtask 6.1
│   │   └──         - [ ] Handle edge cases
│   ├── 📄 checkbox_test.txt (257 tokens, 33 lines)
│   │   ├── - [ ] fix phone number format +1
│   │   ├── - [ ] add forgot password
│   │   ├── - [ ] ? add email verification
│   │   ├── - [ ] store token the right way
│   │   ├── - [ ] test nesting of checkboxes
│   │   ├── - [ ] user can use option to buy ticket at 2-referred price
│   │   ├── - [ ] CTA refer 2 people to get instant lower price
│   │   └── - [ ] form to send referrals
│   ├── 📄 environment.test.ts (197 tokens, 19 lines)
│   │   ├── environment:
│   │   ├──    production
│   │   ├──    cognitoUserPoolId
│   │   ├──    cognitoAppClientId
│   │   └──    apiurl
│   ├── 📄 hello_world.pyi (22 tokens, 3 lines)
│   │   ├── @final
│   │   │   class dtype(Generic[_DTypeScalar_co])
│   │   └──     names: None | tuple
│   ├── 📄 k8s_test.yaml (140 tokens, 37 lines)
│   │   ├── apps/v1.Deployment -> my-app
│   │   ├── v1.Service -> my-service
│   │   └── v1.ConfigMap -> my-config
│   ├── 📄 Makefile (714 tokens, 84 lines)
│   │   ├── include dotenv/dev.env
│   │   ├── .PHONY: dev
│   │   ├── dev
│   │   ├── services-down
│   │   ├── services-stop: services-down
│   │   ├── define CHECK_POSTGRES
│   │   ├── damage-report
│   │   ├── tail-logs
│   │   └── cloud
│   ├── 📄 requirements_test.txt (29 tokens, 10 lines)
│   │   ├── psycopg2-binary
│   │   ├── pytest
│   │   ├── coverage
│   │   ├── flask
│   │   ├── flask_cors
│   │   ├── stripe
│   │   ├── pyjwt
│   │   ├── cognitojwt
│   │   └── flask-lambda
│   ├── 📄 rust_todo_test.rs (92 tokens, 26 lines)
│   │   ├── TODO: This todo tests parse_todo
│   │   ├── enum Color
│   │   ├── struct Point
│   │   ├── trait Drawable
│   │   ├──     fn draw(&self)
│   │   ├── impl Drawable for Point
│   │   ├──     fn draw(&self)
│   │   └── fn main()
│   ├── 📄 sql_test.sql (270 tokens, 51 lines)
│   │   ├── CREATE TABLE promoters
│   │   ├──    user_id serial PRIMARY KEY,
│   │   ├──    type varchar(20) NOT NULL,
│   │   ├──    username varchar(20) NOT NULL,
│   │   ├──    password varchar(20) NOT NULL,
│   │   ├──    email varchar(30) NOT NULL,
│   │   ├──    phone varchar(20) NOT NULL,
│   │   ├──    promocode varchar(20),
│   │   ├──    info json,
│   │   ├──    going text[],
│   │   ├──    invites text[],
│   │   ├──    balance integer NOT NULL,
│   │   ├──    rewards text[],
│   │   ├──    created timestamp
│   │   ├── CREATE TABLE events
│   │   ├──    event_id serial PRIMARY KEY,
│   │   ├──    name varchar(64) NOT NULL,
│   │   ├──    date varchar(64) NOT NULL,
│   │   ├──    location varchar(64) NOT NULL,
│   │   ├──    performer varchar(64) NOT NULL,
│   │   ├──    rewards json,
│   │   └──    created timestamp
│   ├── 📄 standard-app-routing.module.ts (100 tokens, 16 lines)
│   │   └── const routes: Routes = [
│   │         { path: '', component: HomeComponent },
│   │         {
│   │           path: 'heroes',
│   │           component: HeroesListComponent,
│   │           children: [
│   │             { path: ':id', component: HeroDetailComponent },
│   │             { path: 'new', component: HeroFormComponent },
│   │           ],
│   │         },
│   │         { path: '**', component: PageNotFoundComponent },
│   │       ];
│   ├── 📄 test.env (190 tokens, 25 lines)
│   │   ├── PROMO_PATH
│   │   ├── PRODUCTION
│   │   ├── SQL_SCHEMA_PATH
│   │   ├── DB_LOGS
│   │   ├── DB_LOG
│   │   ├── PGPASSWORD
│   │   ├── PGDATABASE
│   │   ├── PGHOST
│   │   ├── PGPORT
│   │   ├── PGUSER
│   │   ├── SERVER_LOG
│   │   ├── SERVER_LOGS
│   │   ├── API_URL
│   │   ├── APP_LOGS
│   │   ├── APP_LOG
│   │   ├── APP_URL
│   │   ├── COGNITO_USER_POOL_ID
│   │   ├── COGNITO_APP_CLIENT_ID
│   │   ├── AWS_REGION
│   │   └── STRIPE_SECRET_KEY
│   ├── 📄 testJsonSchema.json (421 tokens, 48 lines)
│   │   ├── $schema: http://json-schema.org/draft-07/schema#
│   │   ├── type: object
│   │   ├── title: random_test
│   │   └── description: A promoter's activites related to events
│   ├── 📄 testPackage.json (349 tokens, 43 lines)
│   │   ├── name: 'promo-app'
│   │   ├── version: 0.0.0
│   │   ├── scripts:
│   │   ├──     ng: 'ng'
│   │   ├──     start: 'ng serve'
│   │   ├──     build: 'ng build'
│   │   ├──     watch: 'ng build --watch --configuration development'
│   │   └──     test: 'ng test'
│   └── 📄 tickets.component.ts (7,160 tokens, 903 lines)
│       ├── interface EnrichedTicket extends Ticket
│       ├── interface SpinConfig
│       ├── interface RotationState
│       ├── interface SpeakInput
│       ├── const formatSpeakInput = (input: SpeakInput): string =>
│       ├── function hourToSpeech(hour: number, minute: number, period: string): string
│       ├── export class TicketsComponent implements AfterViewInit
│       ├──   speak(input: SpeakInput)
│       ├──   speakEvent(ticket: EnrichedTicket): void
│       ├──   formatEvent(ticket: EnrichedTicket): string
│       ├──   speakVenue(ticket: EnrichedTicket): void
│       ├──   formatDate(date: Date, oneLiner: boolean = false): string
│       ├──   formatDateForSpeech(date: Date): string
│       ├──   async spinQRCode(
│       │       event: PointerEvent,
│       │       config: SpinConfig = DEFAULT_SPIN_CONFIG
│       │     )
│       ├──   private animateRotation(
│       │       imgElement: HTMLElement,
│       │       targetRotation: number,
│       │       config: SpinConfig,
│       │       cleanup: () => void
│       │     )
│       ├──     const animate = (currentTime: number) =>
│       ├──         requestAnimationFrame(animate)
│       ├──         cleanup()
│       ├──     requestAnimationFrame(animate)
│       ├──   private getNext90Degree(currentRotation: number): number
│       ├──   private getCurrentRotation(matrix: string): number
│       ├──   ngAfterViewInit()
│       ├──       const mouseEnterListener = () =>
│       ├──       const mouseLeaveListener = () =>
│       ├──   ngOnDestroy()
│       ├──   toggleColumn(event: MatOptionSelectionChange, column: string)
│       ├──   adjustColumns(event?: Event)
│       ├──   onResize(event: Event)
│       ├──   async ngOnInit()
│       ├──   async loadTickets(): Promise<void>
│       ├──   onDateRangeChange(
│       │       type: "start" | "end",
│       │       event: MatDatepickerInputEvent<Date>
│       │     )
│       ├──   applyFilter(column: string): void
│       ├──   formatDateForComparison(date: Date): string
│       ├──   constructor(private renderer: Renderer2)
│       ├──   onFilterChange(event: Event, column: string)
│       ├──   onLatitudeChange(event: Event)
│       ├──   onLongitudeChange(event: Event)
│       ├──   onRadiusChange(event: Event)
│       ├──   sortData(sort: Sort): void
│       ├──   onRowClick(event: Event, row: any)
│       ├── function isDate(value: Date | undefined | null): value is Date
│       ├── function isNonNullNumber(value: number | null): value is number
│       ├── function hasLocation(
│       │     ticket: any
│       │   ): ticket is
│       ├── const create_faker_ticket = async () =>
│       ├── function compare(a: number | string, b: number | string, isAsc: boolean)
│       ├── function compare_dates(a: Date, b: Date, isAsc: boolean)
│       ├── async function mockMoreTickets(): Promise<Ticket[]>
│       ├── const mockTickets = async () =>
│       └── const renderQRCode = async (text: String): Promise<string> =>
├── 📁 group6 (1 folder, 13 files) 
│   ├── 📄 catastrophic.c (5,339 tokens, 754 lines)
│   │   ├── TODO: technically we should use a proper parser
│   │   ├── struct Point
│   │   ├──     int x;
│   │   ├──     int y;
│   │   ├── struct Point getOrigin()
│   │   ├── float mul_two_floats(float x1, float x2)
│   │   ├── enum days
│   │   ├──     SUN,
│   │   ├──     MON,
│   │   ├──     TUE,
│   │   ├──     WED,
│   │   ├──     THU,
│   │   ├──     FRI,
│   │   ├──     SAT
│   │   ├── enum worker_pool_flags
│   │   ├──         POOL_BH                 = 1 << 0,
│   │   ├──         POOL_MANAGER_ACTIVE     = 1 << 1,
│   │   ├──         POOL_DISASSOCIATED      = 1 << 2,
│   │   ├──         POOL_BH_DRAINING        = 1 << 3,
│   │   ├── enum worker_flags
│   │   ├──         WORKER_DIE              = 1 << 1,
│   │   ├──         WORKER_IDLE             = 1 << 2,
│   │   ├──         WORKER_PREP             = 1 << 3,
│   │   ├──         WORKER_CPU_INTENSIVE    = 1 << 6,
│   │   ├──         WORKER_UNBOUND          = 1 << 7,
│   │   ├──         WORKER_REBOUND          = 1 << 8,
│   │   ├──         WORKER_NOT_RUNNING      = WORKER_PREP | WORKER_CPU_INTENSIVE |
│   │   │                                     WORKER_UNBOUND | WORKER_REBOUND,
│   │   ├── struct worker_pool
│   │   ├──   raw_spinlock_t    lock;
│   │   ├──   int      cpu;
│   │   ├──   int      node;
│   │   ├──   int      id;
│   │   ├──   unsigned int    flags;
│   │   ├──   unsigned long    watchdog_ts;
│   │   ├──   bool      cpu_stall;
│   │   ├──   int      nr_running;
│   │   ├──   struct list_head  worklist;
│   │   ├──   int      nr_workers;
│   │   ├──   int      nr_idle;
│   │   ├──   struct list_head  idle_list;
│   │   ├──   struct timer_list  idle_timer;
│   │   ├──   struct work_struct      idle_cull_work;
│   │   ├──   struct timer_list  mayday_timer;
│   │   ├──   struct worker    *manager;
│   │   ├──   struct list_head  workers;
│   │   ├──   struct ida    worker_ida;
│   │   ├──   struct workqueue_attrs  *attrs;
│   │   ├──   struct hlist_node  hash_node;
│   │   ├──   int      refcnt;
│   │   ├──   struct rcu_head    rcu;
│   │   ├── long add_two_longs(long x1, long x2)
│   │   ├── double multiplyByTwo(double num)
│   │   ├── char getFirstCharacter(char *str)
│   │   ├── void greet(Person p)
│   │   ├── typedef struct
│   │   ├──     char name[50];
│   │   ├── } Person;
│   │   ├── typedef struct PersonA
│   │   ├──     char name[50];
│   │   ├── } PersonB;
│   │   ├── int main()
│   │   ├── int* getArrayStart(int arr[], int size)
│   │   ├── long complexFunctionWithMultipleArguments(
│   │   │       int param1,
│   │   │       double param2,
│   │   │       char *param3,
│   │   │       struct Point point
│   │   │   )
│   │   ├── keyPattern *ACLKeyPatternCreate(sds pattern, int flags)
│   │   ├── sds sdsCatPatternString(sds base, keyPattern *pat)
│   │   ├── static int ACLCheckChannelAgainstList(list *reference, const char *channel, int channellen, int is_pattern)
│   │   ├──     while((ln = listNext(&li)))
│   │   ├── static struct config
│   │   ├──     aeEventLoop *el;
│   │   ├──     cliConnInfo conn_info;
│   │   ├──     const char *hostsocket;
│   │   ├──     int tls;
│   │   ├──     struct cliSSLconfig sslconfig;
│   │   ├── } config;
│   │   ├── class Person
│   │   ├──     std::string name;
│   │   ├── public:
│   │   ├──     Person(std::string n) : name(n)
│   │   ├──     void greet()
│   │   ├── void globalGreet()
│   │   ├── int main()
│   │   ├── void printMessage(const std::string &message)
│   │   ├── template<typename T>
│   │   │   void printVector(const std::vector<T>& vec)
│   │   ├── struct foo
│   │   ├──     char x;
│   │   ├──     struct foo_in
│   │   ├──           char* y;
│   │   ├──           short z;
│   │   ├──     } inner;
│   │   ├── struct Point
│   │   ├──     int x, y;
│   │   ├──     Point(int x, int y) : x(x), y(y)
│   │   ├── class Animal
│   │   ├──   public:
│   │   ├──     Animal(const std::string &name) : name(name)
│   │   ├──     virtual void speak() const
│   │   ├──     virtual ~Animal()
│   │   ├── protected:
│   │   ├──     std::string name;
│   │   ├── class Dog : public Animal
│   │   ├──   public:
│   │   ├──     Dog(const std::string &name) : Animal(name)
│   │   ├──     void speak() const override
│   │   ├── class Cat : public Animal
│   │   ├──   public:
│   │   ├──     Cat(const std::string &name) : Animal(name)
│   │   ├──     void speak() const override
│   │   ├── class CatDog: public Animal, public Cat, public Dog
│   │   ├──   public:
│   │   ├──       CatDog(const std::string &name) : Animal(name)
│   │   ├──       int meow_bark()
│   │   ├── nb::bytes BuildRnnDescriptor(int input_size, int hidden_size, int num_layers,
│   │   │                                int batch_size, int max_seq_length, float dropout,
│   │   │                                bool bidirectional, bool cudnn_allow_tf32,
│   │   │              int workspace_size, int reserve_space_size)
│   │   ├── int main()
│   │   ├── enum ECarTypes
│   │   ├──   Sedan,
│   │   ├──   Hatchback,
│   │   ├──   SUV,
│   │   ├──   Wagon
│   │   ├── ECarTypes GetPreferredCarType()
│   │   ├── enum ECarTypes : uint8_t
│   │   ├──   Sedan,
│   │   ├──   Hatchback,
│   │   ├──   SUV = 254,
│   │   ├──   Hybrid
│   │   ├── enum class ECarTypes : uint8_t
│   │   ├──   Sedan,
│   │   ├──   Hatchback,
│   │   ├──   SUV = 254,
│   │   ├──   Hybrid
│   │   ├── void myFunction(string fname, int age)
│   │   ├── template <typename T> T cos(T)
│   │   ├── template <typename T> T sin(T)
│   │   ├── template <typename T> T sqrt(T)
│   │   ├── template<typename T> struct VLEN
│   │   ├── template<typename T> class arr
│   │   ├──   private:
│   │   ├──     static T *ralloc(size_t num)
│   │   ├──     static void dealloc(T *ptr)
│   │   ├──     static T *ralloc(size_t num)
│   │   ├──     static void dealloc(T *ptr)
│   │   ├──   public:
│   │   ├──     arr() : p(0), sz(0)
│   │   ├──     arr(size_t n) : p(ralloc(n)), sz(n)
│   │   ├──     arr(arr &&other)
│   │   │         : p(other.p), sz(other.sz)
│   │   ├──     ~arr()
│   │   ├──     void resize(size_t n)
│   │   ├──     T &operator[](size_t idx)
│   │   ├──     T *data()
│   │   ├──     size_t size() const
│   │   ├── class Buffer
│   │   ├──  private:
│   │   ├──   void* ptr_;
│   │   ├── std::tuple<array, array, array> quantize(
│   │   │       const array& w,
│   │   │       int group_size,
│   │   │       int bits,
│   │   │       StreamOrDevice s)
│   │   ├── #define PY_SSIZE_T_CLEAN
│   │   ├── #define PLATFORM_IS_X86
│   │   ├── #define PLATFORM_WINDOWS
│   │   ├── #define GETCPUID(a, b, c, d, a_inp, c_inp)
│   │   ├── static int GetXCR0EAX()
│   │   ├── #define GETCPUID(a, b, c, d, a_inp, c_inp)
│   │   ├── static int GetXCR0EAX()
│   │   ├──   asm("XGETBV" : "=a"(eax), "=d"(edx) : "c"(0))
│   │   ├── static void ReportMissingCpuFeature(const char* name)
│   │   ├── static PyObject *CheckCpuFeatures(PyObject *self, PyObject *args)
│   │   ├── static PyObject *CheckCpuFeatures(PyObject *self, PyObject *args)
│   │   ├── static PyMethodDef cpu_feature_guard_methods[]
│   │   ├── static struct PyModuleDef cpu_feature_guard_module
│   │   ├── #define EXPORT_SYMBOL __declspec(dllexport)
│   │   ├── #define EXPORT_SYMBOL __attribute__ ((visibility("default")))
│   │   ├── EXPORT_SYMBOL PyMODINIT_FUNC PyInit_cpu_feature_guard(void)
│   │   ├── typedef struct
│   │   ├──     GPT2Config config;
│   │   ├──     ParameterTensors params;
│   │   ├──     size_t param_sizes[NUM_PARAMETER_TENSORS];
│   │   ├──     float* params_memory;
│   │   ├──     size_t num_parameters;
│   │   ├──     ParameterTensors grads;
│   │   ├──     float* grads_memory;
│   │   ├──     float* m_memory;
│   │   ├──     float* v_memory;
│   │   ├──     ActivationTensors acts;
│   │   ├──     size_t act_sizes[NUM_ACTIVATION_TENSORS];
│   │   ├──     float* acts_memory;
│   │   ├──     size_t num_activations;
│   │   ├──     ActivationTensors grads_acts;
│   │   ├──     float* grads_acts_memory;
│   │   ├──     int batch_size;
│   │   ├──     int seq_len;
│   │   ├──     int* inputs;
│   │   ├──     int* targets;
│   │   ├──     float mean_loss;
│   │   └── } GPT2;
│   ├── 📄 cpp_examples_impl.cc (60 tokens, 10 lines)
│   │   ├── PYBIND11_MODULE(cpp_examples, m)
│   │   └──     m.def("add", &add<int>, "An example function to add two numbers.")
│   ├── 📄 cpp_examples_impl.cu (37 tokens, 10 lines)
│   │   ├── template <typename T>
│   │   │   T add(T a, T b)
│   │   └── template <>
│   │       int add<int>(int a, int b)
│   ├── 📄 cpp_examples_impl.h (22 tokens, 6 lines)
│   │   ├── template <typename T>
│   │   │   T add(T a, T b)
│   │   └── template <>
│   │       int add<int>(int, int)
│   ├── 📄 fractal.thy (1,712 tokens, 147 lines)
│   │   ├── Title:      fractal.thy
│   │   ├── Author:     Isabelle/HOL Contributors!
│   │   ├── Author:     edge cases r us
│   │   ├── theory Simplified_Ring
│   │   ├── section ‹Basic Algebraic Structures›
│   │   ├── class everything = nothing + itself
│   │   ├── subsection ‹Monoids›
│   │   ├── definition ring_hom :: "[('a, 'm) ring_scheme, ('b, 'n) ring_scheme] => ('a => 'b) set"
│   │   ├── fun example_fun :: "nat ⇒ nat"
│   │   ├── locale monoid =
│   │   │     fixes G (structure)
│   │   │     assumes m_closed: "⟦x ∈ carrier G; y ∈ carrier G⟧ ⟹  x ⊗ y ∈ carrier G"
│   │   │       and m_assoc: "⟦x ∈ carrier G; y ∈ carrier G; z ∈ carrier G⟧ ⟹  (x ⊗ y) ⊗ z = x ⊗ (y ⊗ z)"
│   │   │       and one_closed: "𝟭 ∈ carrier G"
│   │   │       and l_one: "x ∈ carrier G ⟹  𝟭 ⊗ x = x"
│   │   │       and r_one: "x ∈ carrier G ⟹  x ⊗ 𝟭 = x"
│   │   ├── subsection ‹Groups›
│   │   ├── locale group = monoid +
│   │   │     assumes Units_closed: "x ∈ Units G ⟹  x ∈ carrier G"
│   │   │       and l_inv_ex: "x ∈ carrier G ⟹  ∃ y ∈ carrier G. y ⊗ x = 𝟭"
│   │   │       and r_inv_ex: "x ∈ carrier G ⟹  ∃ y ∈ carrier G. x ⊗ y = 𝟭"
│   │   ├── subsection ‹Rings›
│   │   ├── locale ring = abelian_group R + monoid R +
│   │   │     assumes l_distr: "⟦x ∈ carrier R; y ∈ carrier R; z ∈ carrier R⟧ ⟹  (x ⊕ y) ⊗ z = x ⊗ z ⊕ y ⊗ z"
│   │   │       and r_distr: "⟦x ∈ carrier R; y ∈ carrier R; z ∈ carrier R⟧ ⟹  z ⊗ (x ⊕ y) = z ⊗ x ⊕ z ⊗ y"
│   │   ├── locale commutative_ring = ring +
│   │   │     assumes m_commutative: "⟦x ∈ carrier R; y ∈ carrier R⟧ ⟹  x ⊗ y = y ⊗ x"
│   │   ├── locale domain = commutative_ring +
│   │   │     assumes no_zero_divisors: "⟦a ⊗ b = 𝟬; a ∈ carrier R; b ∈ carrier R⟧ ⟹  a = 𝟬 ∨ b = 𝟬"
│   │   ├── locale field = domain +
│   │   │     assumes inv_ex: "x ∈ carrier R - {𝟬} ⟹  inv x ∈ carrier R"
│   │   ├── subsection ‹Morphisms›
│   │   ├── lemma example_lemma: "example_fun n = n"
│   │   ├── qualified lemma gcd_0:
│   │   │     "gcd a 0 = normalize a"
│   │   ├── lemma abelian_monoidI:
│   │   │     fixes R (structure)
│   │   │         and f :: "'edge::{} ⇒ 'case::{}"
│   │   │     assumes "⋀x y. ⟦ x ∈ carrier R; y ∈ carrier R ⟧ ⟹  x ⊕ y ∈ carrier R"
│   │   │         and "𝟬 ∈ carrier R"
│   │   │         and "⋀x y z. ⟦ x ∈ carrier R; y ∈ carrier R; z ∈ carrier R ⟧ ⟹  (x ⊕ y) ⊕ z = x ⊕ (y ⊕ z)"
│   │   │     shows "abelian_monoid R"
│   │   ├── lemma euclidean_size_gcd_le1 :
│   │   │     assumes "a ≠ 0"
│   │   │     shows "euclidean_size (gcd a b) ≤ euclidean_size a"
│   │   ├── theorem Residue_theorem:
│   │   │     fixes S pts::"complex set" and f::"complex ⇒ complex"
│   │   │       and g::"real ⇒ complex"
│   │   │     assumes "open S" "connected S" "finite pts" and
│   │   │             holo:"f holomorphic_on S-pts" and
│   │   │             "valid_path g" and
│   │   │             loop:"pathfinish g = pathstart g" and
│   │   │             "path_image g ⊆ S-pts" and
│   │   │             homo:"∀z. (z ∉ S) ⟶  winding_number g z  = 0"
│   │   │     shows "contour_integral g f = 2 * pi * 𝗂 *(∑p ∈ pts. winding_number g p * residue f p)"
│   │   ├── corollary fps_coeff_residues_bigo':
│   │   │     fixes f :: "complex ⇒ complex" and r :: real
│   │   │     assumes exp: "f has_fps_expansion F"
│   │   │     assumes "open A" "connected A" "cball 0 r ⊆ A" "r > 0" 
│   │   │     assumes "f holomorphic_on A - S" "S ⊆ ball 0 r" "finite S" "0 ∉ S"
│   │   │     assumes "eventually (λn. g n = -(∑z ∈ S. residue (λz. f z / z ^ Suc n) z)) sequentially"
│   │   │                (is "eventually (λn. _ = -?g' n) _")
│   │   │     shows   "(λn. fps_nth F n - g n) ∈ O(λn. 1 / r ^ n)" (is "(λn. ?c n - _) ∈ O(_)")
│   │   └── end
│   ├── 📄 Microsoft.PowerShell_profile.ps1 (3,346 tokens, 497 lines)
│   │   ├── function Log($message)
│   │   ├── function Remove-ChocolateyFromPath
│   │   ├── function Show-Profiles
│   │   ├── function Show-Path
│   │   ├── function Show-Error($err)
│   │   ├── function Get-ScoopPackagePath
│   │   ├──   param(
│   │   │       [Parameter(Mandatory = $true)]
│   │   │       $PackageName)
│   │   ├── function Check-Command
│   │   ├──   param(
│   │   │       [Parameter(Mandatory = $true)]
│   │   │       $Name)
│   │   ├── function Add-ToPath
│   │   ├──   param(
│   │   │       [Parameter(Mandatory = $true)]
│   │   │       $PathToAdd)
│   │   ├── function Install-Scoop
│   │   ├── function Scoop-Install
│   │   ├──   param(
│   │   │       [Parameter(Mandatory = $true)]
│   │   │       $Name,
│   │   │       $PathToAdd)
│   │   ├── function Start-CondaEnv
│   │   ├── function Install-PipPackage
│   │   ├──   param(
│   │   │           [Parameter(Mandatory = $true)]
│   │   │       $PackageName)
│   │   ├── function Install-VSBuildTools
│   │   ├── function Install-Crate
│   │   ├──   param(
│   │   │           [Parameter(Mandatory = $true)]
│   │   │       $CrateName)
│   │   ├── function Get-ScoopVersion
│   │   ├── function Get-Version
│   │   ├──     param(
│   │   │           [Parameter(Mandatory = $true)]
│   │   │           $ExecutablePath,
│   │   │           $ExecutableName)
│   │   ├── function Show-Requirements
│   │   ├──   function Measure-Status
│   │   ├──     param(
│   │   │         [Parameter(Mandatory = $true)]
│   │   │         $Name)
│   │   ├── function Find-Profile
│   │   ├── function Edit-Profile
│   │   ├── function Set-Profile
│   │   └── function Show-Profile
│   ├── 📄 python_complex_class.py (10 tokens, 2 lines)
│   │   └── class Box(Space[NDArray[Any]])
│   ├── 📄 ramda__cloneRegExp.js (173 tokens, 9 lines)
│   │   └── export default function _cloneRegExp(pattern)
│   ├── 📄 ramda_prop.js (646 tokens, 85 lines)
│   │   ├── /**
│   │   │    * Returns a function that when supplied an object returns the indicated
│   │   │    * property of that object, if it exists.
│   │   │    * @category Object
│   │   │    * @typedefn Idx = String | Int | Symbol
│   │   │    * @sig Idx -> {s: a} -> a | Undefined
│   │   │    * @param {String|Number} p The property name or array index
│   │   │    * @param {Object} obj The object to query
│   │   │    * @return {*} The value at `obj.p`.
│   │   │    */
│   │   │   var prop = _curry2(function prop(p, obj)
│   │   ├── /**
│   │   │    * Solves equations of the form a * x = b
│   │   │    * @param {{
│   │   │    *  z: number
│   │   │    * }} x
│   │   │    */
│   │   │   function foo(x)
│   │   ├── /**
│   │   │    * Deconstructs an array field from the input documents to output a document for each element.
│   │   │    * Each output document is the input document with the value of the array field replaced by the element.
│   │   │    * @category Object
│   │   │    * @sig String -> {k: } -> [{k: v}]
│   │   │    * @param {String} key The key to determine which property of the object should be unwound.
│   │   │    * @param {Object} object The object containing the list to unwind at the property named by the key.
│   │   │    * @return {List} A list of new objects, each having the given key associated to an item from the unwound list.
│   │   │    */
│   │   │   var unwind = _curry2(function(key, object)
│   │   └──   return _map(function(item)
│   ├── 📄 tensorflow_flags.h (7,628 tokens, 668 lines)
│   │   ├── TF_DECLARE_FLAG('test_only_experiment_1')
│   │   ├── TF_DECLARE_FLAG('test_only_experiment_2')
│   │   ├── TF_DECLARE_FLAG('enable_nested_function_shape_inference'):
│   │   │     Allow ops such as tf.cond to invoke the ShapeRefiner on their nested functions.
│   │   ├── TF_DECLARE_FLAG('enable_quantized_dtypes_training'):
│   │   │     Set quantized dtypes, like tf.qint8, to be trainable.
│   │   ├── TF_DECLARE_FLAG('graph_building_optimization'):
│   │   │     Optimize graph building for faster tf.function tracing.
│   │   ├── TF_DECLARE_FLAG('saved_model_fingerprinting'):
│   │   │     Add fingerprint to SavedModels.
│   │   ├── TF_DECLARE_FLAG('more_stack_traces'):
│   │   │     Enable experimental code that preserves and propagates graph node stack traces in C++.
│   │   ├── TF_DECLARE_FLAG('publish_function_graphs'):
│   │   │     Enables the publication of partitioned function graphs via StatsPublisherInterface. Disabling this flag can reduce
│   │   │   memory consumption.
│   │   ├── TF_DECLARE_FLAG('enable_aggressive_constant_replication'):
│   │   │     Replicate constants across CPU devices and even for local CPUs within the same task if available.
│   │   ├── TF_DECLARE_FLAG('enable_colocation_key_propagation_in_while_op_lowering'):
│   │   │     If true, colocation key attributes for the ops will be propagated during while op lowering to switch/merge ops.
│   │   ├── Flag('tf_xla_auto_jit'):
│   │   │     Control compilation of operators into XLA computations on CPU and GPU devices.  0 = use ConfigProto setting; -1 = 
│   │   │   off; 1 = on for things very likely to be improved; 2 = on for everything; (experimental) fusible = only for 
│   │   │   Tensorflow operations that XLA knows how to fuse. If set to single-gpu(<N>) then this resolves to <N> for single-GPU
│   │   │   graphs (graphs that have at least one node placed on a GPU and no more than one GPU is in use through the entire 
│   │   │   graph) and 0 otherwise.  Experimental.
│   │   ├── Flag('tf_xla_min_cluster_size'):
│   │   │     Minimum number of operators in an XLA compilation. Ignored for operators placed on an XLA device or operators 
│   │   │   explicitly marked for compilation.
│   │   ├── Flag('tf_xla_max_cluster_size'):
│   │   │     Maximum number of operators in an XLA compilation.
│   │   ├── Flag('tf_xla_cluster_exclude_ops'):
│   │   │     (experimental) Exclude the operations from auto-clustering. If multiple, separate them with commas. Where, 
│   │   │   Some_other_ops.
│   │   ├── Flag('tf_xla_clustering_debug'):
│   │   │     Dump graphs during XLA compilation.
│   │   ├── Flag('tf_xla_cpu_global_jit'):
│   │   │     Enables global JIT compilation for CPU via SessionOptions.
│   │   ├── Flag('tf_xla_clustering_fuel'):
│   │   │     Places an artificial limit on the number of ops marked as eligible for clustering.
│   │   ├── Flag('tf_xla_disable_deadness_safety_checks_for_debugging'):
│   │   │     Disable deadness related safety checks when clustering (this is unsound).
│   │   ├── Flag('tf_xla_disable_resource_variable_safety_checks_for_debugging'):
│   │   │     Disable resource variables related safety checks when clustering (this is unsound).
│   │   ├── Flag('tf_xla_deterministic_cluster_names'):
│   │   │     Causes the function names assigned by auto clustering to be deterministic from run to run.
│   │   ├── Flag('tf_xla_persistent_cache_directory'):
│   │   │     If non-empty, JIT-compiled executables are saved to and loaded from the specified file system directory path. 
│   │   │   Empty by default.
│   │   ├── Flag('tf_xla_persistent_cache_device_types'):
│   │   │     If non-empty, the persistent cache will only be used for the specified devices (comma separated). Each device type
│   │   │   should be able to be converted to.
│   │   ├── Flag('tf_xla_persistent_cache_read_only'):
│   │   │     If true, the persistent cache will be read-only.
│   │   ├── Flag('tf_xla_disable_strict_signature_checks'):
│   │   │     If true, entires loaded into the XLA compile cache will not have their signatures checked strictly. Defaults to 
│   │   │   false.
│   │   ├── Flag('tf_xla_persistent_cache_prefix'):
│   │   │     Specifies the persistance cache prefix. Default is.
│   │   ├── Flag('tf_xla_sparse_core_disable_table_stacking'):
│   │   │     Disable table stacking for all the tables passed to the SparseCore mid level API.
│   │   ├── Flag('tf_xla_sparse_core_minibatch_max_division_level'):
│   │   │     Max level of division to split input data into minibatches.
│   │   ├── Flag('tf_xla_sparse_core_stacking_mem_limit_bytes'):
│   │   │     If non-zero, limits the size of the activations for a given table to be below these many bytes.
│   │   ├── Flag('tf_xla_sparse_core_stacking_table_shard_limit_bytes'):
│   │   │     If non-zero, limits the size of any table shard to be below these many bytes.
│   │   ├── Flag('always_specialize')
│   │   ├── Flag('cost_driven_async_parallel_for')
│   │   ├── Flag('enable_crash_reproducer')
│   │   ├── Flag('log_query_of_death')
│   │   ├── Flag('vectorize')
│   │   ├── Flag('tf_xla_enable_lazy_compilation')
│   │   ├── Flag('tf_xla_print_cluster_outputs'):
│   │   │     If true then insert Print nodes to print out values produced by XLA clusters.
│   │   ├── Flag('tf_xla_check_cluster_input_numerics'):
│   │   │     If true then insert CheckNumerics nodes to check all cluster inputs.
│   │   ├── Flag('tf_xla_check_cluster_output_numerics'):
│   │   │     If true then insert CheckNumerics nodes to check all cluster outputs.
│   │   ├── Flag('tf_xla_disable_constant_folding'):
│   │   │     If true then disables constant folding on TF graph before XLA compilation.
│   │   ├── Flag('tf_xla_disable_full_embedding_pipelining'):
│   │   │     If true then disables full embedding pipelining and instead use strict SparseCore / TensorCore sequencing.
│   │   ├── Flag('tf_xla_embedding_parallel_iterations'):
│   │   │     If >0 then use this many parallel iterations in embedding_pipelining and embedding_sequency. By default, use the 
│   │   │   parallel_iterations on the original model WhileOp.
│   │   ├── Flag('tf_xla_compile_on_demand'):
│   │   │     Switch a device into 'on-demand' mode, where instead of autoclustering ops are compiled one by one just-in-time.
│   │   ├── Flag('tf_xla_enable_xla_devices'):
│   │   │     Generate XLA_* devices, where placing a computation on such a device forces compilation by XLA. Deprecated.
│   │   ├── Flag('tf_xla_always_defer_compilation')
│   │   ├── Flag('tf_xla_async_compilation'):
│   │   │     When lazy compilation is enabled, asynchronous compilation starts the cluster compilation in the background, and 
│   │   │   the fallback path is executed until the compilation has finished.
│   │   ├── Flag('tf_xla_use_device_api_for_xla_launch'):
│   │   │     If true, uses Device API (PjRt) for single device compilation and execution of functions marked for JIT 
│   │   │   compilation i.e. jit_compile=True. Defaults to false.
│   │   ├── Flag('tf_xla_use_device_api_for_compile_on_demand'):
│   │   │     If true, uses Device API (PjRt) for compiling and executing ops one by one in 'on-demand' mode. Defaults to false.
│   │   ├── Flag('tf_xla_use_device_api_for_auto_jit'):
│   │   │     If true, uses Device API (PjRt) for compilation and execution when auto-clustering is enabled. Defaults to false.
│   │   ├── Flag('tf_xla_use_device_api'):
│   │   │     If true, uses Device API (PjRt) for compilation and execution of ops one-by-one in 'on-demand' mode, for functions
│   │   │   marked for JIT compilation, or when auto-clustering is enabled. Defaults to false.
│   │   ├── Flag('tf_xla_enable_device_api_for_gpu'):
│   │   │     If true, uses Device API (PjRt) for TF GPU device. This is a helper flag so that individual tests can turn on PjRt
│   │   │   for GPU specifically.
│   │   ├── Flag('tf_xla_call_module_disabled_checks'):
│   │   │     A comma-sepated list of directives specifying the safety checks to be skipped when compiling XlaCallModuleOp. See 
│   │   │   the op documentation for the recognized values.
│   │   ├── Flag('tf_mlir_enable_mlir_bridge'):
│   │   │     Enables experimental MLIR-Based TensorFlow Compiler Bridge.
│   │   ├── Flag('tf_mlir_enable_merge_control_flow_pass'):
│   │   │     Enables MergeControlFlow pass for MLIR-Based TensorFlow Compiler Bridge.
│   │   ├── Flag('tf_mlir_enable_convert_control_to_data_outputs_pass'):
│   │   │     Enables MLIR-Based TensorFlow Compiler Bridge.
│   │   ├── Flag('tf_mlir_enable_strict_clusters'):
│   │   │     Do not allow clusters that have cyclic control dependencies.
│   │   ├── Flag('tf_mlir_enable_multiple_local_cpu_devices'):
│   │   │     Enable multiple local CPU devices. CPU ops which are outside compiled inside the tpu cluster will also be 
│   │   │   replicated across multiple cpu devices.
│   │   ├── Flag('tf_dump_graphs_in_tfg'):
│   │   │     When tf_dump_graphs_in_tfg is true, graphs after transformations are dumped in MLIR TFG dialect and not in 
│   │   │   GraphDef.
│   │   ├── Flag('tf_mlir_enable_generic_outside_compilation'):
│   │   │     Enables OutsideCompilation passes for MLIR-Based TensorFlow Generic Compiler Bridge.
│   │   ├── Flag('tf_mlir_enable_tpu_variable_runtime_reformatting_pass'):
│   │   │     Enables TPUVariableRuntimeReformatting pass for MLIR-Based TensorFlow Compiler Bridge. This enables weight update 
│   │   │   sharding and creates TPUReshardVariables ops.
│   │   ├── TF_PY_DECLARE_FLAG('test_only_experiment_1')
│   │   ├── TF_PY_DECLARE_FLAG('test_only_experiment_2')
│   │   ├── TF_PY_DECLARE_FLAG('enable_nested_function_shape_inference')
│   │   ├── TF_PY_DECLARE_FLAG('enable_quantized_dtypes_training')
│   │   ├── TF_PY_DECLARE_FLAG('graph_building_optimization')
│   │   ├── TF_PY_DECLARE_FLAG('op_building_optimization')
│   │   ├── TF_PY_DECLARE_FLAG('saved_model_fingerprinting')
│   │   ├── TF_PY_DECLARE_FLAG('tf_shape_default_int64')
│   │   ├── TF_PY_DECLARE_FLAG('more_stack_traces')
│   │   ├── TF_PY_DECLARE_FLAG('publish_function_graphs')
│   │   ├── TF_PY_DECLARE_FLAG('enable_aggressive_constant_replication')
│   │   ├── TF_PY_DECLARE_FLAG('enable_colocation_key_propagation_in_while_op_lowering')
│   │   ├── #define TENSORFLOW_CORE_CONFIG_FLAG_DEFS_H_
│   │   ├── class Flags
│   │   ├──  public:
│   │   ├── bool SetterForXlaAutoJitFlag(const string& value)
│   │   ├── bool SetterForXlaCallModuleDisabledChecks(const string& value)
│   │   ├── void AppendMarkForCompilationPassFlagsInternal(std::vector<Flag>* flag_list)
│   │   ├── void AllocateAndParseJitRtFlags()
│   │   ├── void AllocateAndParseFlags()
│   │   ├── void ResetFlags()
│   │   ├── bool SetXlaAutoJitFlagFromFlagString(const string& value)
│   │   ├── BuildXlaOpsPassFlags* GetBuildXlaOpsPassFlags()
│   │   ├── MarkForCompilationPassFlags* GetMarkForCompilationPassFlags()
│   │   ├── XlaSparseCoreFlags* GetXlaSparseCoreFlags()
│   │   ├── XlaDeviceFlags* GetXlaDeviceFlags()
│   │   ├── XlaOpsCommonFlags* GetXlaOpsCommonFlags()
│   │   ├── XlaCallModuleFlags* GetXlaCallModuleFlags()
│   │   ├── MlirCommonFlags* GetMlirCommonFlags()
│   │   ├── void ResetJitCompilerFlags()
│   │   ├── const JitRtFlags& GetJitRtFlags()
│   │   ├── ConfigProto::Experimental::MlirBridgeRollout GetMlirBridgeRolloutState(
│   │   │       std::optional<const ConfigProto> config_proto)
│   │   ├── void AppendMarkForCompilationPassFlags(std::vector<Flag>* flag_list)
│   │   ├── void DisableXlaCompilation()
│   │   ├── void EnableXlaCompilation()
│   │   ├── bool FailOnXlaCompilation()
│   │   ├── #define TF_PY_DECLARE_FLAG(flag_name)
│   │   └── PYBIND11_MODULE(flags_pybind, m)
│   ├── 📄 test.f (181 tokens, 30 lines)
│   │   ├── MODULE basic_mod
│   │   ├──     TYPE :: person
│   │   │           CHARACTER(LEN=50) :: name
│   │   │           INTEGER :: age
│   │   │       END TYPE person
│   │   ├──     SUBROUTINE short_hello(happy, path)
│   │   │       END SUBROUTINE short_hello
│   │   ├──     SUBROUTINE long_hello(
│   │   │           p,
│   │   │           message
│   │   │       )
│   │   │       END SUBROUTINE long_hello
│   │   ├── END MODULE basic_mod
│   │   └── PROGRAM HelloFortran
│   │       END PROGRAM HelloFortran
│   ├── 📄 torch.rst (60 tokens, 8 lines)
│   │   ├── # libtorch (C++-only)
│   │   └── - Building libtorch using Python
│   └── 📄 yc.html (9,063 tokens, 169 lines)
├── 📁 group7 (1 folder, 3 files) 
│   ├── 📄 absurdly_huge.jsonl (8,347 tokens, 126 lines)
│   │   ├── SMILES: str
│   │   ├── Yield: float
│   │   ├── Temperature: int
│   │   ├── Pressure: float
│   │   ├── Solvent: str
│   │   ├── Success: bool
│   │   ├── Reaction_Conditions: dict
│   │   ├── Products: list
│   │   └── EdgeCasesMissed: None
│   ├── 📄 angular_crud.ts (1,192 tokens, 148 lines)
│   │   ├── interface DBCommand<T = any>
│   │   ├── export class IndexedDbService
│   │   ├──     constructor()
│   │   ├──     async create_connection({ db_name = 'client_db', table_name }: DBCommand)
│   │   ├──                 upgrade(db)
│   │   ├──     async create_model({ db_name, table_name, model }: DBCommand)
│   │   ├──         verify_matching({ table_name, model })
│   │   ├──     async read_key({ db_name, table_name, key }: DBCommand)
│   │   ├──     async update_model({ db_name, table_name, model }: DBCommand)
│   │   ├──         verify_matching({ table_name, model })
│   │   ├──     async delete_key({ db_name, table_name, key }: DBCommand)
│   │   ├──     async list_table({
│   │   │           db_name,
│   │   │           table_name,
│   │   │           where,
│   │   │       }: DBCommand & { where?: { : string | number } })
│   │   └──     async search_table(criteria: SearchCriteria)
│   └── 📄 structure.py (400 tokens, 92 lines)
│       ├── @runtime_checkable
│       │   class DataClass(Protocol)
│       ├──     __dataclass_fields__: dict
│       ├── class MyInteger(Enum)
│       ├──     ONE = 1
│       ├──     TWO = 2
│       ├──     THREE = 42
│       ├── class MyString(Enum)
│       ├──     AAA1 = "aaa"
│       ├──     BB_B = """edge
│       │   case"""
│       ├── @dataclass(frozen=True, slots=True, kw_only=True)
│       │   class Tool
│       ├──     name: str
│       ├──     description: str
│       ├──     input_model: DataClass
│       ├──     output_model: DataClass
│       ├──     def execute(self, *args, **kwargs)
│       ├──     @property
│       │       def edge_case(self) -> str
│       ├──     def should_still_see_me(self, x: bool = True) -> "Tool"
│       ├── @dataclass
│       │   class MyInput[T]
│       ├──     name: str
│       ├──     rank: MyInteger
│       ├──     serial_n: int
│       ├── @dataclass
│       │   class Thingy
│       ├──     is_edge_case: bool
│       ├── @dataclass
│       │   class MyOutput
│       ├──     orders: str
│       ├── class MyTools(Enum)
│       ├──     TOOL_A = Tool(
│       │           name="complicated",
│       │           description="edge case!",
│       │           input_model=MyInput[Thingy],
│       │           output_model=MyOutput,
│       │       )
│       ├──     TOOL_B = Tool(
│       │           name="""super
│       │   complicated
│       │   """,
│       │           description="edge case!",
│       │           input_model=MyInput,
│       │           output_model=MyOutput,
│       │       )
│       ├── @final
│       │   class dtype(Generic[_DTypeScalar_co])
│       └──     names: None | tuple
└── 📁 group_lisp (1 folder, 4 files) 
    ├── 📄 clojure_test.clj (682 tokens, 85 lines)
    │   ├── defprotocol P
    │   ├── defrecord Person
    │   ├── defn -main
    │   ├── ns bion.likes_trees
    │   ├── def repo-url
    │   ├── defn config
    │   ├── defmacro with-os
    │   └── defrecord SetFullElement
    ├── 📄 LispTest.lisp (25 tokens, 6 lines)
    │   ├── defstruct person
    │   └── defun greet
    ├── 📄 racket_struct.rkt (14 tokens, 1 line)
    │   └── struct point
    └── 📄 test_scheme.scm (360 tokens, 44 lines)
        ├── define topological-sort
        ├──   define table
        ├──   define queue
        ├──   define result
        ├──   define set-up
        └──   define traverse

tree_plus v(1.0.61) ignore=('group_todo',) globs=() syntax=False paths=('tests/more_languages',)
9 folder(s), 84 file(s), 7,158 line(s), 68,018 token(s) in 0.68 second(s).

```
<!-- t2-end -->
## Got Globs?

<!-- t3-start -->
```sh
tree_plus -g "*.*s" -i group_todo tests/more_languages
📁 more_languages (7 folders, 17 files) 
├── 📁 group1 (1 folder, 2 files) 
│   ├── 📄 test.js (757 tokens, 154 lines)
│   │   ├── class MyClass
│   │   ├──   myMethod()
│   │   ├──   async asyncMethod(a, b)
│   │   ├──   methodWithDefaultParameters(a = 5, b = 10)
│   │   ├──   multilineMethod(
│   │   │       c,
│   │   │       d
│   │   │     )
│   │   ├──   multilineMethodWithDefaults(
│   │   │       t = "tree",
│   │   │       p = "plus"
│   │   │     )
│   │   ├── function myFunction(param1, param2)
│   │   ├── function multilineFunction(
│   │   │     param1,
│   │   │     param2
│   │   │   )
│   │   ├── const arrowFunction = () =>
│   │   ├── const parametricArrow = (a, b) =>
│   │   ├── function ()
│   │   ├── function outerFunction(outerParam)
│   │   ├──   function innerFunction(innerParam)
│   │   ├──   innerFunction("inner")
│   │   ├── const myObject = {
│   │   ├──   myMethod: function (stuff)
│   │   ├── let myArrowObject = {
│   │   ├──   myArrow: ({
│   │   │       a,
│   │   │       b,
│   │   │       c,
│   │   │     }) =>
│   │   ├── const myAsyncArrowFunction = async () =>
│   │   ├── function functionWithRestParameters(...args)
│   │   ├── const namedFunctionExpression = function myNamedFunction()
│   │   ├── const multilineArrowFunction = (
│   │   │     a,
│   │   │     b
│   │   │   ) =>
│   │   ├── function functionReturningFunction()
│   │   ├──   return function ()
│   │   ├── function destructuringOnMultipleLines({
│   │   │     a,
│   │   │     b,
│   │   │   })
│   │   ├── const arrowFunctionWithDestructuring = ({ a, b }) =>
│   │   ├── const multilineDestructuringArrow = ({
│   │   │     a,
│   │   │     b,
│   │   │   }) =>
│   │   ├── async function asyncFunctionWithErrorHandling()
│   │   ├── class Car
│   │   ├──   constructor(brand)
│   │   ├──   present()
│   │   ├── class Model extends Car
│   │   ├──   constructor(brand, mod)
│   │   ├──     super(brand)
│   │   └──   show()
│   └── 📄 test.ts (832 tokens, 165 lines)
│       ├── type MyType
│       ├── interface MyInterface
│       ├── class TsClass
│       ├──   myMethod()
│       ├──   myMethodWithArgs(param1: string, param2: number): void
│       ├──   static myStaticMethod<T>(param: T): T
│       ├──   multilineMethod(
│       │       c: number,
│       │       d: number
│       │     ): number
│       ├──   multilineMethodWithDefaults(
│       │       t: string = "tree",
│       │       p: string = "plus"
│       │     ): string
│       ├── export class AdvancedComponent implements MyInterface
│       ├──   async myAsyncMethod(
│       │       a: string,
│       │       b: number,
│       │       c: string
│       │     ): Promise<void>
│       ├──   genericMethod<T, U>(
│       │       arg1: T,
│       │       arg2: U
│       │     ): [T, U]
│       ├── export class TicketsComponent implements MyInterface
│       ├──   async myAsyncMethod({ a, b, c }: { a: String; b: Number; c: String })
│       ├── function tsFunction()
│       ├── function tsFunctionSigned(
│       │     param1: number,
│       │     param2: number
│       │   ): void
│       ├── export default async function tsFunctionComplicated<A, B, C>({
│       │     a = 1 | 2,
│       │     b = "bob",
│       │     c = async () => "charlie",
│       │   }: {
│       │     a: number;
│       │     b: string;
│       │     c: () => Promise<string>;
│       │   }): Promise<string>
│       ├──   return("Standalone function with parameters")
│       ├── const tsArrowFunctionSigned = ({
│       │     a,
│       │     b,
│       │   }: {
│       │     a: number;
│       │     b: string;
│       │   }) =>
│       ├── export const tsComplicatedArrow = async ({
│       │     a = 1 | 2,
│       │     b = "bob",
│       │     c = async () => "charlie",
│       │   }: {
│       │     a: number;
│       │     b: string;
│       │     c: () => Promise<string>;
│       │   }): Promise<string> =>
│       ├── const arrowFunction = () =>
│       ├── const arrow = (a: String, b: Number) =>
│       ├── const asyncArrowFunction = async () =>
│       ├── const asyncArrow = async (a: String, b: Number) =>
│       ├── let weirdArrow = () =>
│       ├── const asyncPromiseArrow = async (): Promise<void> =>
│       ├── let myWeirdArrowSigned = (x: number): number =>
│       ├── class Person
│       ├──   constructor(private firstName: string, private lastName: string)
│       ├──   getFullName(): string
│       ├──   describe(): string
│       ├── class Employee extends Person
│       ├──   constructor(
│       │       firstName: string,
│       │       lastName: string,
│       │       private jobTitle: string
│       │     )
│       ├──     super(firstName, lastName)
│       ├──   describe(): string
│       ├── interface Shape
│       └── interface Square extends Shape
├── 📁 group3 (1 folder, 1 file) 
│   └── 📄 csharp_test.cs (957 tokens, 146 lines)
│       ├── public interface IExcelTemplate
│       ├──     void LoadTemplate(string templateFilePath)
│       ├──     void LoadData(Dictionary<string, string> data)
│       ├──     void ModifyCell(string cellName, string value)
│       ├──     void SaveToFile(string filePath)
│       ├── public interface IGreet
│       ├──     void Greet()
│       ├── public enum WeekDays
│       ├── public delegate void DisplayMessage(string message)
│       ├── public struct Address
│       ├── public static class HelperFunctions
│       ├──     public static void PrintMessage(string message)
│       ├──     public static int AddNumbers(int a, int b)
│       ├── namespace HelloWorldApp
│       ├──     class Person : IGreet
│       ├──         public Person(string name, int age)
│       ├──         public void Greet()
│       ├──     class HelloWorld
│       ├──         static void Main(string[] args)
│       ├── namespace TemplateToExcelServer.Template
│       ├──     public interface ITemplateObject
│       ├──         string[,] GetContent()
│       ├──         string[] GetContentArray()
│       ├──         string[] GetFormat()
│       ├──         int? GetFormatLength()
│       ├──         TemplateObject SetContent(string[,] Content)
│       ├──         TemplateObject SetContentArray(string[] value)
│       ├──         TemplateObject SetFormat(string[] Header)
│       ├──         TemplateObject SetNameOfReport(
│       │               ReadOnlyMemory<byte> ReportName,
│       │               int[] EdgeCase)
│       ├──         TemplateObject SetSheetName(ReadOnlyMemory<byte> SheetName)
│       ├── public class BankAccount(string accountID, string owner)
│       ├──     public override string ToString() =>
│       ├── var IncrementBy = (int source, int increment = 1) =>
│       ├── Func<int, int, int> add = (x, y) =>
│       ├── button.Click += (sender, args) =>
│       ├── public Func<int, int> GetMultiplier(int factor)
│       ├── public void Method(
│       │           int param1,
│       │           int param2,
│       │           int param3,
│       │           int param4,
│       │           int param5,
│       │           int param6,
│       │       )
│       ├── System.Net.ServicePointManager.ServerCertificateValidationCallback +=
│       │       (se, cert, chain, sslerror) =>
│       ├── class ServerCertificateValidation
│       ├──     public bool OnRemoteCertificateValidation(
│       │           object se,
│       │           X509Certificate cert,
│       │           X509Chain chain,
│       │           SslPolicyErrors sslerror
│       │       )
│       ├── s_downloadButton.Clicked += async (o, e) =>
│       ├── [HttpGet, Route("DotNetCount")]
│       └── static public async Task<int> GetDotNetCount(string URL)
├── 📁 group4 (1 folder, 3 files) 
│   ├── 📄 haskell_test.hs (414 tokens, 41 lines)
│   │   ├── data Person
│   │   ├── greet :: Person -> String
│   │   └── resolveVariables ::
│   │         forall m fragments.
│   │         (MonadError QErr m, Traversable fragments) =>
│   │         Options.BackwardsCompatibleNullInNonNullableVariables ->
│   │         [G.VariableDefinition] ->
│   │         GH.VariableValues ->
│   │         [G.Directive G.Name] ->
│   │         G.SelectionSet fragments G.Name ->
│   │         m
│   │           ( [G.Directive Variable],
│   │             G.SelectionSet fragments Variable
│   │           )
│   ├── 📄 rust_test.rs (974 tokens, 188 lines)
│   │   ├── enum Days
│   │   ├── struct Point
│   │   ├── impl Point
│   │   ├──     fn get_origin() -> Point
│   │   ├── struct Person
│   │   ├── impl Person
│   │   ├──     fn greet(&self)
│   │   ├── fn add_two_longs(x1: i64, x2: i64) -> i64
│   │   ├── fn add_two_longs_longer(
│   │   │       x1: i64,
│   │   │       x2: i64,
│   │   │   ) -> i64
│   │   ├── fn multiply_by_two(num: f64) -> f64
│   │   ├── fn get_first_character(s: &str) -> Option<char>
│   │   ├── trait Drawable
│   │   ├──     fn draw(&self)
│   │   ├── impl Drawable for Point
│   │   ├──     fn draw(&self)
│   │   ├── fn main()
│   │   ├── pub struct VisibleStruct
│   │   ├── mod my_module
│   │   ├──     pub struct AlsoVisibleStruct<T>(T, T)
│   │   ├── macro_rules! say_hello
│   │   ├── #
│   │   │   macro_rules! hello_tree_plus
│   │   ├── pub mod lib
│   │   ├──     pub mod interfaces
│   │   ├──     mod engine
│   │   ├── pub fn flow<S1, S2, S3, S4, E, T, L>(
│   │   │       source: S1,
│   │   │       extractor: E,
│   │   │       inbox: S2,
│   │   │       transformer: T,
│   │   │       outbox: S3,
│   │   │       loader: L,
│   │   │       sink: &mut S4,
│   │   │   ) -> Result<(), Box<dyn Error>>
│   │   │   where
│   │   │       S1: Extractable,
│   │   │       S2: Extractable + Loadable,
│   │   │       S3: Extractable + Loadable,
│   │   │       S4: Loadable,
│   │   │       E: Extractor<S1, S2>,
│   │   │       T: Transformer<S2, S3>,
│   │   │       L: Loader<S3, S4>
│   │   ├── trait Container
│   │   ├──     fn items(&self) -> impl Iterator<Item = Widget>
│   │   ├── trait HttpService
│   │   ├──     async fn fetch(&self, url: Url) -> HtmlBody
│   │   ├── struct Pair<T, U>
│   │   ├── trait Transformer<T>
│   │   ├──     fn transform(&self, input: T) -> T
│   │   ├── impl<T: std::ops::Add<Output = T> + Copy> Transformer<T> for Pair<T, T>
│   │   ├──     fn transform(&self, input: T) -> T
│   │   ├── fn main()
│   │   ├── async fn handle_get(State(pool): State<PgPool>) -> Result<Html<String>, (StatusCode, String)> 
│   │   │   where
│   │   │       Bion: Cool
│   │   ├── #
│   │   │   macro_rules! unit
│   │   └──             fn insert(
│   │                       &mut self,
│   │                       key: (),
│   │                       value: $unit_dtype,
│   │                   ) -> Result<Option<$unit_dtype>, ETLError>
│   └── 📄 test_fsharp.fs (92 tokens, 27 lines)
│       ├── module TestFSharp
│       ├── type Person = {
│       ├── let add x y =
│       ├── let multiply 
│       │       (x: int) 
│       │       (y: int): int =
│       ├── let complexFunction
│       │       (a: int)
│       │       (b: string)
│       │       (c: float)
│       │       : (int * string) option =
│       └── type Result<'T> =
├── 📁 group5 (1 folder, 8 files) 
│   ├── 📄 app-routing.module.ts (287 tokens, 28 lines)
│   │   ├── const routes: Routes = [
│   │   │       { path: '', redirectTo: 'login', pathMatch: 'full' },
│   │   │       { path: '*', redirectTo: 'login' },
│   │   │       { path: 'home', component: HomeComponent },
│   │   │       { path: 'login', component: LoginComponent },
│   │   │       { path: 'register', component: RegisterComponent },
│   │   │       { path: 'events', component: EventsComponent },
│   │   │       { path: 'invites', component: InvitesComponent },
│   │   │       { path: 'rewards', component: RewardsComponent },
│   │   │       { path: 'profile', component: ProfileComponent },
│   │   │   ];
│   │   └── export class AppRoutingModule
│   ├── 📄 app.component.spec.ts (410 tokens, 47 lines)
│   │   ├── describe 'AppComponent'
│   │   ├──     it should create the app
│   │   ├──     it should welcome the user
│   │   ├──     it should welcome 'Jimbo'
│   │   └──     it should request login if not logged in
│   ├── 📄 app.component.ts (271 tokens, 45 lines)
│   │   ├── export class AppComponent
│   │   ├──   constructor(
│   │   │       private http: HttpClient,
│   │   │       private loginService: LoginService,
│   │   │       private stripeService: StripeService
│   │   │     )
│   │   ├──   constructor(private loginService: LoginService)
│   │   ├──   checkSession()
│   │   ├──   async goToEvent(event_id: string)
│   │   └──   valInvitedBy(event: any, event_id: string)
│   ├── 📄 app.module.ts (374 tokens, 43 lines)
│   │   ├── @NgModule({
│   │   │       declarations: [
│   │   │           AppComponent,
│   │   │           HomeComponent,
│   │   │           LoginComponent,
│   │   │           RegisterComponent,
│   │   │           EventsComponent,
│   │   │           InvitesComponent,
│   │   │           RewardsComponent,
│   │   │           ProfileComponent
│   │   └── export class AppModule
│   ├── 📄 environment.test.ts (197 tokens, 19 lines)
│   │   ├── environment:
│   │   ├──    production
│   │   ├──    cognitoUserPoolId
│   │   ├──    cognitoAppClientId
│   │   └──    apiurl
│   ├── 📄 rust_todo_test.rs (92 tokens, 26 lines)
│   │   ├── TODO: This todo tests parse_todo
│   │   ├── enum Color
│   │   ├── struct Point
│   │   ├── trait Drawable
│   │   ├──     fn draw(&self)
│   │   ├── impl Drawable for Point
│   │   ├──     fn draw(&self)
│   │   └── fn main()
│   ├── 📄 standard-app-routing.module.ts (100 tokens, 16 lines)
│   │   └── const routes: Routes = [
│   │         { path: '', component: HomeComponent },
│   │         {
│   │           path: 'heroes',
│   │           component: HeroesListComponent,
│   │           children: [
│   │             { path: ':id', component: HeroDetailComponent },
│   │             { path: 'new', component: HeroFormComponent },
│   │           ],
│   │         },
│   │         { path: '**', component: PageNotFoundComponent },
│   │       ];
│   └── 📄 tickets.component.ts (7,160 tokens, 903 lines)
│       ├── interface EnrichedTicket extends Ticket
│       ├── interface SpinConfig
│       ├── interface RotationState
│       ├── interface SpeakInput
│       ├── const formatSpeakInput = (input: SpeakInput): string =>
│       ├── function hourToSpeech(hour: number, minute: number, period: string): string
│       ├── export class TicketsComponent implements AfterViewInit
│       ├──   speak(input: SpeakInput)
│       ├──   speakEvent(ticket: EnrichedTicket): void
│       ├──   formatEvent(ticket: EnrichedTicket): string
│       ├──   speakVenue(ticket: EnrichedTicket): void
│       ├──   formatDate(date: Date, oneLiner: boolean = false): string
│       ├──   formatDateForSpeech(date: Date): string
│       ├──   async spinQRCode(
│       │       event: PointerEvent,
│       │       config: SpinConfig = DEFAULT_SPIN_CONFIG
│       │     )
│       ├──   private animateRotation(
│       │       imgElement: HTMLElement,
│       │       targetRotation: number,
│       │       config: SpinConfig,
│       │       cleanup: () => void
│       │     )
│       ├──     const animate = (currentTime: number) =>
│       ├──         requestAnimationFrame(animate)
│       ├──         cleanup()
│       ├──     requestAnimationFrame(animate)
│       ├──   private getNext90Degree(currentRotation: number): number
│       ├──   private getCurrentRotation(matrix: string): number
│       ├──   ngAfterViewInit()
│       ├──       const mouseEnterListener = () =>
│       ├──       const mouseLeaveListener = () =>
│       ├──   ngOnDestroy()
│       ├──   toggleColumn(event: MatOptionSelectionChange, column: string)
│       ├──   adjustColumns(event?: Event)
│       ├──   onResize(event: Event)
│       ├──   async ngOnInit()
│       ├──   async loadTickets(): Promise<void>
│       ├──   onDateRangeChange(
│       │       type: "start" | "end",
│       │       event: MatDatepickerInputEvent<Date>
│       │     )
│       ├──   applyFilter(column: string): void
│       ├──   formatDateForComparison(date: Date): string
│       ├──   constructor(private renderer: Renderer2)
│       ├──   onFilterChange(event: Event, column: string)
│       ├──   onLatitudeChange(event: Event)
│       ├──   onLongitudeChange(event: Event)
│       ├──   onRadiusChange(event: Event)
│       ├──   sortData(sort: Sort): void
│       ├──   onRowClick(event: Event, row: any)
│       ├── function isDate(value: Date | undefined | null): value is Date
│       ├── function isNonNullNumber(value: number | null): value is number
│       ├── function hasLocation(
│       │     ticket: any
│       │   ): ticket is
│       ├── const create_faker_ticket = async () =>
│       ├── function compare(a: number | string, b: number | string, isAsc: boolean)
│       ├── function compare_dates(a: Date, b: Date, isAsc: boolean)
│       ├── async function mockMoreTickets(): Promise<Ticket[]>
│       ├── const mockTickets = async () =>
│       └── const renderQRCode = async (text: String): Promise<string> =>
├── 📁 group6 (1 folder, 2 files) 
│   ├── 📄 ramda__cloneRegExp.js (173 tokens, 9 lines)
│   │   └── export default function _cloneRegExp(pattern)
│   └── 📄 ramda_prop.js (646 tokens, 85 lines)
│       ├── /**
│       │    * Returns a function that when supplied an object returns the indicated
│       │    * property of that object, if it exists.
│       │    * @category Object
│       │    * @typedefn Idx = String | Int | Symbol
│       │    * @sig Idx -> {s: a} -> a | Undefined
│       │    * @param {String|Number} p The property name or array index
│       │    * @param {Object} obj The object to query
│       │    * @return {*} The value at `obj.p`.
│       │    */
│       │   var prop = _curry2(function prop(p, obj)
│       ├── /**
│       │    * Solves equations of the form a * x = b
│       │    * @param {{
│       │    *  z: number
│       │    * }} x
│       │    */
│       │   function foo(x)
│       ├── /**
│       │    * Deconstructs an array field from the input documents to output a document for each element.
│       │    * Each output document is the input document with the value of the array field replaced by the element.
│       │    * @category Object
│       │    * @sig String -> {k: } -> [{k: v}]
│       │    * @param {String} key The key to determine which property of the object should be unwound.
│       │    * @param {Object} object The object containing the list to unwind at the property named by the key.
│       │    * @return {List} A list of new objects, each having the given key associated to an item from the unwound list.
│       │    */
│       │   var unwind = _curry2(function(key, object)
│       └──   return _map(function(item)
└── 📁 group7 (1 folder, 1 file) 
    └── 📄 angular_crud.ts (1,192 tokens, 148 lines)
        ├── interface DBCommand<T = any>
        ├── export class IndexedDbService
        ├──     constructor()
        ├──     async create_connection({ db_name = 'client_db', table_name }: DBCommand)
        ├──                 upgrade(db)
        ├──     async create_model({ db_name, table_name, model }: DBCommand)
        ├──         verify_matching({ table_name, model })
        ├──     async read_key({ db_name, table_name, key }: DBCommand)
        ├──     async update_model({ db_name, table_name, model }: DBCommand)
        ├──         verify_matching({ table_name, model })
        ├──     async delete_key({ db_name, table_name, key }: DBCommand)
        ├──     async list_table({
        │           db_name,
        │           table_name,
        │           where,
        │       }: DBCommand & { where?: { : string | number } })
        └──     async search_table(criteria: SearchCriteria)

tree_plus v(1.0.61) ignore=('group_todo',) globs=('*.*s',) syntax=False paths=('tests/more_languages',)
7 folder(s), 17 file(s), 2,090 line(s), 14,928 token(s) in 0.18 second(s).

```
<!-- t3-end -->

## Languages Todo:

Help me **add to** and **priorize** this list of languages to support!

- [Open a `tree_plus` GitHub Issue](https://github.com/bionicles/tree_plus/issues/new), or
- [Contact Bion on X](https://twitter.com/flesheatingemu)

<!-- t4-start -->
```sh
tree_plus tests/more_languages/group_todo
📁 group_todo (1 folder, 12 files) 
├── 📄 AAPLShaders.metal (5,780 tokens, 566 lines)
├── 📄 crystal_test.cr (48 tokens, 15 lines)
├── 📄 dart_test.dart (108 tokens, 24 lines)
├── 📄 elixir_test.exs (39 tokens, 10 lines)
├── 📄 forward.frag (739 tokens, 87 lines)
├── 📄 forward.vert (359 tokens, 48 lines)
├── 📄 nodemon.json (118 tokens, 20 lines)
├── 📄 sas_test.sas (97 tokens, 22 lines)
├── 📄 test_setup_py.test (133 tokens, 24 lines)
├── 📄 testTypings.d.ts (158 tokens, 23 lines)
├── 📄 vba_test.bas (67 tokens, 16 lines)
└── 📄 wgsl_test.wgsl (94 tokens, 17 lines)

tree_plus v(1.0.61) ignore=() globs=() syntax=False paths=('tests/more_languages/group_todo',)
1 folder(s), 12 file(s), 872 line(s), 7,740 token(s) in 0.03 second(s).

```
<!-- t4-end -->

## Oppose Unfair Business Practices

Please consider contacting the authorities to report the issue described in this document:

[California OpenAI Complaint - Customer Noncompete Clause](https://www.tinyurl.com/cali-openai-complaint)

_Remember_: **Your Voice Matters!**

## License

MIT or Apache 2.0, at your option.