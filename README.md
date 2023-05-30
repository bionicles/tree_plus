# Tree Plus

A simple CLI tool for developers to show a `tree` enhanced with token counts, line counts, and source code components. 

## Usage:
```sh
(py310) bion@WIN-QVRBL09D89C:~/hax/tree_plus$ python -m src.cli ./src
./src (4761 tokens, 613 lines)
┗━━ 📁 src (4761 tokens, 613 lines)
    ┣━━ 📄 __init__.py (51 tokens, 8 lines)
    ┣━━ 📄 count_tokens_lines.py (327 tokens, 48 lines)
    ┃   ┣━━ class TokenLineCount
    ┃   ┣━━ def count_tokens_lines
    ┃   ┗━━ def count_directory_tokens_lines
    ┣━━ 📄 cli.py (882 tokens, 108 lines)
    ┃   ┣━━ def main
    ┃   ┣━━ def tree_plus
    ┃   ┗━━ def tree_to_string
    ┣━━ 📄 traverse_directory.py (254 tokens, 35 lines)
    ┃   ┗━━ def traverse_directory
    ┗━━ 📄 parse_file.py (3247 tokens, 414 lines)
        ┣━━ def parse_file
        ┣━━ def extract_nodes
        ┣━━ def is_typing_construct
        ┣━━ def is_builtin_type
        ┣━━ def parse_py
        ┣━━ def parse_cobol
        ┣━━ def parse_java
        ┣━━ def parse_julia
        ┣━━ def parse_kotlin
        ┣━━ def parse_lisp
        ┣━━ def parse_lua
        ┣━━ def parse_objective_c
        ┣━━ def parse_ocaml
        ┣━━ def parse_apl
        ┣━━ def parse_matlab
        ┣━━ def parse_js
        ┣━━ def parse_md
        ┗━━ def parse_todo
```

# Software Engineering Prompt(s) (SWE)
- [x] **1. Discuss the Problem, Users, and Ethical Considerations**
  - "AI, let's define the problem we're solving, identify our users, and address any ethical implications of our solution."
- [x] **2. Create and Refine the High-Level Design**
  - "AI, help me create a design document. We need to ensure illegal states are unrepresentable and refine our design based on potential feedback."
- [x] **3. Setup Tools and Define Testing Strategy**
  - "AI, let's identify the tools we need and write unit tests that focus on important behaviors and edge cases."
- [x] **4. Validate System with End-to-End Tests**
  - "AI, let's write end-to-end tests to validate the entire system works from the user's perspective."
- [] **5. Write, Review, and Integrate Code**
  - "AI, let's start writing SOLID code, review it together, and commit it frequently to integrate with the existing codebase."
- [] **6. Document and Get Feedback**
  - "AI, let's update our design document with final details, write user-level documentation, and actively seek out feedback."
- [] **7. Reflect and Learn**
  - "AI, let's conduct a retrospective analysis to learn from our failures and successes."


# 1. Discuss the Problem, Users, and Ethical Considerations

### Problem:
Extend Linux `tree` util to provide a deeper look into contents of Python, JavaScript, Markdown files, and more. It should display software project architecture, major components in the output, and display the number of lines of code and number of OpenAI tokens within each directory and file.

### Goals:
- Extend the capabilities of the existing `tree` command.
- Parse Python, JavaScript, and Markdown files to identify and display major components.
- Calculate and display the number of OpenAI tokens and lines of code in each file and directory.

### Components:
1. **`traverse_directory` Module:** Responsible for traversing through directories and files in a specified location. This will form the base operation similar to the `tree` command.

2. **`parse_file` Module:** Responsible for parsing Python, JavaScript, and Markdown files. It will identify major components in these files.

3. **`count_tokens_lines` Module:** Responsible for counting the number of lines and OpenAI tokens in each file and directory.

3. **`cli` Module:** Responsible for the `click` cli and the `tree_plus` function it invokes to build a `rich.tree.Tree`, and print the tree.

### Data Flow:
- The user provides one or more directories as input.
- `traverse_directory` traverses directories and identifies subdirectories and files.
- For each identified Python, JavaScript, or Markdown file, `parse_file` identifies major components and  `count_tokens_lines`  counts the lines of code and OpenAI tokens.
- The output is organized and displayed to the user by `cli`.

### Potential Refinements Based on Feedback:
This design assumes that the primary goal is to parse specific file types and calculate specific metrics. Feedback might suggest that the tool should be extensible to support more languages, include more metrics, or provide filtering capabilities. Adjustments to the design would be made accordingly.

### Ensuring Illegal States are Unrepresentable:
To ensure illegal states are unrepresentable, we'll validate input paths, handle file reading errors, and safely manage instances where permission to a file or directory is denied. The tool will not attempt to parse unsupported file types.

# 3. Setup Tools and Define Testing Strategy

**Tools Needed:**
- **Python**: The language we will use to build the tool.
- **pyTest**: to write our unit tests and end-to-end tests.
- **GitHub**: A platform for version control where we can commit changes frequently and track the development of the project.
- **CI/CD Pipeline (GitHub Actions)**: For automating tests and ensuring our code is always deployable.
- **click**: To create a command-line interface for the tool.
- **tiktoken**: A Python package for counting OpenAI tokens.
- **rich.tree.Tree**: to display the directory and file hierarchy.

**Testing Strategy:**

1. **Unit Tests**: We will write unit tests for each module and each major function within the modules. Important behaviors and edge cases should be tested. Here are some examples:
    - Test that the File Traversal Module correctly identifies directories and files.
    - Test that the File Parsing Module correctly identifies major components in Python, JavaScript, and Markdown files.
    - Test that the Token Counting Module correctly counts lines of code and OpenAI tokens.
    - Test that errors are handled correctly when encountering unreadable files or directories.

2. **Integration Tests**: These tests will ensure that the modules work correctly together. For example, testing that the File Traversal Module correctly passes files to the File Parsing Module and the Token Counting Module.

3. **End-to-End Tests**: These tests will be written to ensure that the entire system works from the user's perspective. They will simulate real user scenarios, such as passing in multiple directories at once.

4. **Performance Tests**: We will also test how the tool performs with large directory structures to ensure it remains performant and doesn't consume excessive resources.

# Distribution
To distribute your package on PyPI, you'll need to create a PyPI account, then create a source distribution and/or a wheel distribution, then upload your distributions with twine.