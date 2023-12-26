# Tree Plus

**A `tree` util enhanced with tokens, lines, and components. Why? To see the big picture!**

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

**Supported Operating Systems:**

![Ubuntu](https://img.shields.io/badge/Supports-Ubuntu-orange)
![MacOS](https://img.shields.io/badge/Supports-MacOS-orange)
![Windows](https://img.shields.io/badge/Supports-Windows-orange)

**Support Free, Open-Source Software:**

[![Support Tree Plus](https://img.shields.io/badge/Support%20Tree%20Plus-8A2BE2)](https://www.buymeacoffee.com/bionicles)


`pip install -U tree_plus`

## Example Output:
- [ ] Demonstrate Parsed Checkboxes
<!-- t1-start -->
```sh

```
<!-- t1-end -->
- [x] Demonstrate Parsed Checkboxes

Here's how `tree_plus --help` looks (`-h` and `-H` both also work) 
<!-- t5-start -->
```sh

```
<!-- t5-end -->

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

## Moar Languages

<!-- t2-start -->
```sh


```
<!-- t2-end -->
## Got Globs?

<!-- t3-start -->
```sh


```
<!-- t3-end -->

## Languages Todo:

Help me **add to** and **priorize** this list of languages to support!

- [Open a `tree_plus` GitHub Issue](https://github.com/bionicles/tree_plus/issues/new), or
- [Contact Bion on X](https://twitter.com/flesheatingemu)

<!-- t4-start -->
```sh


```
<!-- t4-end -->

## Oppose Unfair Business Practices

Please consider contacting the authorities to report the issue described in this document:

[California OpenAI Complaint - Customer Noncompete Clause](https://www.tinyurl.com/cali-openai-complaint)

_Remember_: **Your Voice Matters!**

## License

MIT or Apache 2.0, at your option.