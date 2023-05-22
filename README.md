# Tree Plus

# Software Engineering Prompt(s) (SWE)
- [x] **1. Discuss the Problem, Users, and Ethical Considerations**
  - "AI, let's define the problem we're solving, identify our users, and address any ethical implications of our solution."
- [x] **2. Create and Refine the High-Level Design**
  - "AI, help me create a design document. We need to ensure illegal states are unrepresentable and refine our design based on potential feedback."
- [x] **3. Setup Tools and Define Testing Strategy**
  - "AI, let's identify the tools we need and write unit tests that focus on important behaviors and edge cases."
- [] **4. Write, Review, and Integrate Code**
  - "AI, let's start writing SOLID code, review it together, and commit it frequently to integrate with the existing codebase."
- [] **5. Validate System with End-to-End Tests**
  - "AI, let's write end-to-end tests to validate the entire system works from the user's perspective."
- [] **6. Document and Get Feedback**
  - "AI, let's update our design document with final details, write user-level documentation, and actively seek out feedback."
- [] **7. Reflect and Learn**
  - "AI, let's conduct a retrospective analysis to learn from our failures and successes."


# 1. Discuss the Problem, Users, and Ethical Considerations

### Problem: 

Current tools for displaying directory and file hierarchies (like the tree command in Linux) do not offer a detailed look into the content of files specifically related to popular programming and markup languages like Python, JavaScript, and Markdown. There's also no easy way to convey a software project's architecture to language model (LLM) systems, such as GPT-3 by OpenAI, at a high level.

### Users: 

Your users will be software developers, software architects, or any individuals who need to get an overview of the structure and content of their projects. This will also include those who need to understand the number of OpenAI tokens in each file, which would be particularly relevant for developers using LLMs.

### Ethical Considerations: 
The tool should provide clear, accurate, and meaningful information, supporting user empowerment and aiding in efficient utilization of LLMs. There should be safeguards against possible misuse, such as respecting directory or file read permissions to avoid exposing sensitive information.

# 2. Create and Refine the High-Level Design

## Initial Design Document**

### Problem:
Extend the Linux `tree` command to provide a deeper look into the contents of Python, JavaScript, Markdown files, and more. It should display software project architecture, major components in the output, and display the number of lines of code and number of OpenAI tokens within each directory and file.

### Goals:
- Extend the capabilities of the existing `tree` command.
- Parse Python, JavaScript, and Markdown files to identify and display major components.
- Calculate and display the number of lines of code in each file and directory.
- Calculate and display the number of OpenAI tokens in each file and directory.

### Components:
1. **File Traversal Module:** Responsible for traversing through directories and files in a specified location. This will form the base operation similar to the `tree` command.

2. **File Parsing Module:** Responsible for parsing Python, JavaScript, and Markdown files. It will identify major components in these files.

3. **Token Counting Module:** Responsible for counting the number of lines and OpenAI tokens in each file and directory.

### Data Flow:
- The user provides one or more directory as input.
- The File Traversal Module traverses the directories and identifies files and subdirectories.
- For each identified Python, JavaScript, or Markdown file, the File Parsing Module identifies major components and the Token Counting Module counts the lines of code and OpenAI tokens.
- The output is then organized and displayed to the user.

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

Next, we can move onto writing the code and continuously integrating it with the existing codebase.