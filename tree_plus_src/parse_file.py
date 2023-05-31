# tree_plus_src/parse_file.py
from typing import List
import collections
import builtins
import typing
import ast
import os
import re


def parse_file(file_path: str) -> List[str]:
    """
    Parse a file and return a List[str] of its major components.
    """
    file_extension = os.path.splitext(file_path)[-1].lower()
    components = []
    try:
        with open(file_path, "r") as file:
            contents = file.read()
    except UnicodeDecodeError as ude:
        print(f"UnicodeDecodeError @ {file_path}: {ude}")
        return components

    if file_extension == ".py":
        components = parse_py(contents)
    elif file_extension in [".js", ".ts", ".jsx", ".tsx"]:
        components = parse_js(contents)
    elif file_extension == ".md":
        components = parse_md(contents)
    elif file_extension == ".cbl":
        components = parse_cobol(contents)
    elif file_extension == ".java":
        components = parse_java(contents)
    elif file_extension == ".jl":
        components = parse_julia(contents)
    elif file_extension == ".kt":
        components = parse_kotlin(contents)
    elif file_extension == ".lisp":
        components = parse_lisp(contents)
    elif file_extension == ".lua":
        components = parse_lua(contents)
    elif file_extension == ".m":
        if "@interface" in contents or "@implementation" in contents:
            components = parse_objective_c(contents)
        elif "classdef" in contents or "methods" in contents:
            components = parse_matlab(contents)
    elif file_extension == ".matlab":
        components = parse_matlab(contents)
    elif file_extension == ".ml":
        components = parse_ocaml(contents)
    elif file_extension == ".apl":
        components = parse_apl(contents)
    elif file_extension == ".pl":
        components = parse_perl(contents)
    elif file_extension == ".php":
        components = parse_php(contents)
    elif file_extension == ".ps1":
        components = parse_powershell(contents)
    return components


def extract_nodes(node, node_type, parent=None):
    result = []
    if isinstance(node, node_type):
        result.append((parent, node))
    for child in ast.iter_child_nodes(node):
        result.extend(extract_nodes(child, node_type, node))
    return result


def is_typing_construct(node):
    if isinstance(node, ast.Name):
        return node.id in typing.__all__
    elif (
        isinstance(node, ast.Attribute)
        and isinstance(node.value, ast.Name)
        and node.value.id == "typing"
    ):
        return node.attr in typing.__all__
    elif isinstance(node, ast.Call) and isinstance(node.func, ast.Name):
        return node.func.id in typing.__all__
    return False


def is_builtin_type(node, parent):
    if isinstance(node, ast.Name) and isinstance(parent, ast.AnnAssign):
        return node.id in dir(builtins) + dir(collections)
    return False


def parse_py(content: str) -> List[str]:
    node = ast.parse(content)
    classes = extract_nodes(node, ast.ClassDef)
    types = [
        n[1].targets[0].id
        for n in extract_nodes(node, ast.Assign)
        if isinstance(n[1].value, ast.Call)
        and (
            is_typing_construct(n[1].value.func)
            or is_builtin_type(n[1].value.func, n[0])
        )
        and not isinstance(n[0], ast.ClassDef)
    ]
    classnames = ["class " + n[1].name for n in classes]
    methods = [
        f"class {n[1].name} -> def {m[1].name}"
        for n in classes
        for m in extract_nodes(n[1], ast.FunctionDef)
    ]
    funcs = extract_nodes(node, ast.FunctionDef)
    funcnames = [
        "def " + n[1].name
        for n in funcs
        if not any(n[1].name == m.split()[-1] for m in methods)
    ]

    return types + classnames + methods + funcnames


def parse_cobol(content: str) -> List[str]:
    division_patterns = [
        (
            r"IDENTIFICATION DIVISION.*\n.*PROGRAM-ID. (\w+)",
            "IDENTIFICATION DIVISION -> PROGRAM-ID. ",
        ),
        (r"DATA DIVISION.*\n.*01 (\w+)", "DATA DIVISION -> 01 "),
        (r"PROCEDURE DIVISION", "PROCEDURE DIVISION"),
    ]
    components = []
    for pattern, division_string in division_patterns:
        matches = re.findall(pattern, content, re.DOTALL)
        for match in matches:
            if division_string == "PROCEDURE DIVISION":
                components.append(division_string)
            else:
                components.append(division_string + match)
    return components


def parse_java(content: str) -> List[str]:
    components = []

    # Find class declarations
    class_pattern = r"\bclass\s+(\w+)\s*"
    class_matches = re.findall(class_pattern, content)

    for class_match in class_matches:
        components.append(f"class {class_match}")

    # Find methods (including constructors)
    method_pattern = (
        r"(public|protected|private|static)?\s*([\w\[\]<>,\s]*)\s+(\w+)\s*\((.*?)\)\s*{"
    )
    method_matches = re.findall(method_pattern, content)

    for method_match in method_matches:
        modifier = method_match[0].strip() if method_match[0] else ""
        return_type = method_match[1].strip()
        method_name = method_match[2].strip()
        params = method_match[3].strip()
        for class_match in class_matches:
            # If the method name matches the class name, it's a constructor
            if class_match == method_name:
                components.append(f"class {class_match} -> {class_match}({params})")
            else:
                leaf = f"class {class_match} -> "
                # Only add a space after modifier if it's not empty
                leaf += f"{modifier} " if modifier else ""
                leaf += f"{return_type} {method_name}({params})"
                components.append(leaf)

    return components


def parse_julia(content: str) -> List[str]:
    components = []

    # Find module declarations
    module_pattern = r"\bmodule\s+(\w+)"
    module_matches = re.findall(module_pattern, content)

    # For each module, add it and its components to the output list
    for module_match in module_matches:
        module_name = module_match
        components.append(f"module {module_name}")

        # Find struct declarations within the module
        struct_pattern = r"\bstruct\s+(\w+)"
        struct_matches = re.findall(struct_pattern, content)
        for struct_match in struct_matches:
            components.append(f"module {module_name} -> struct {struct_match}")

        # Find function definitions within the module
        # Note the addition of `=` in the pattern
        function_pattern = r"(\w+)\s*\((.*?)\)\s*="
        function_matches = re.findall(function_pattern, content)
        for function_match in function_matches:
            function_name = function_match[0]
            parameters = function_match[1]
            components.append(f"module {module_name} -> {function_name}({parameters})")

    return components


def parse_kotlin(content: str) -> List[str]:
    components = []

    # Find class declarations
    class_pattern = r"\bdata class\s+(\w+)\((.*?)\)"
    class_matches = re.findall(class_pattern, content)

    for class_match in class_matches:
        class_name = class_match[0]
        properties = class_match[1]
        components.append(f"data class {class_name}({properties})")

    # Find function definitions
    function_pattern = r"\bfun\s+(\w+)\((.*?)\)"
    function_matches = re.findall(function_pattern, content)

    for function_match in function_matches:
        function_name = function_match[0]
        parameters = function_match[1]
        components.append(f"fun {function_name}({parameters})")

    return components


def parse_lisp(content: str) -> List[str]:
    components = []

    # Find struct declarations
    struct_pattern = r"\(defstruct\s+(\w+)"
    struct_matches = re.findall(struct_pattern, content)

    for struct_match in struct_matches:
        components.append(f"defstruct {struct_match}")

    # Find function declarations
    function_pattern = r"\(defun\s+(\w+)"
    function_matches = re.findall(function_pattern, content)

    for function_match in function_matches:
        components.append(f"defun {function_match}")

    return components


def parse_lua(content: str) -> List[str]:
    components = []

    # Find function declarations, but ignore arguments
    function_pattern = r"\bfunction\s+([\w.]+)\s*\("
    function_matches = re.findall(function_pattern, content)

    for function_match in function_matches:
        function_declaration = f"function {function_match}"
        components.append(function_declaration)

    return components


def parse_objective_c(content: str) -> List[str]:
    components = []

    interface_pattern = r"@interface\s+(\w+).*?:\s+NSObject\s*"
    interface_matches = re.findall(interface_pattern, content, re.DOTALL)

    for interface_match in interface_matches:
        class_name = interface_match
        components.append(f"@interface {class_name}")

    interface_method_pattern = (
        r"@interface\s+\w+.*?:\s+NSObject\s*[-\s]*\((.*?)\)\s+(.*?);\s*"
    )
    interface_method_matches = re.findall(interface_method_pattern, content, re.DOTALL)

    for interface_method_match in interface_method_matches:
        return_type = interface_method_match[0]
        method = interface_method_match[1].rstrip()
        components.append(f"@interface {class_name} -> ({return_type}) {method}")

    implementation_pattern = r"@implementation\s+(\w+)\s*\n"
    implementation_matches = re.findall(implementation_pattern, content, re.DOTALL)

    for implementation_match in implementation_matches:
        class_name = implementation_match
        components.append(f"@implementation {class_name}")

    implementation_method_pattern = r"@implementation\s+\w+\s*\n- \((.*?)\)\s+(.*?){"
    implementation_method_matches = re.findall(
        implementation_method_pattern, content, re.DOTALL
    )

    for implementation_method_match in implementation_method_matches:
        return_type = implementation_method_match[0]
        method = implementation_method_match[1].rstrip()
        components.append(f"@implementation {class_name} -> ({return_type}) {method}")

    function_pattern = r"(void)\s+(\w+)\(\)\s*{"
    function_matches = re.findall(function_pattern, content, re.DOTALL)

    for function_match in function_matches:
        return_type = function_match[0]
        function_name = function_match[1]
        components.append(f"{return_type} {function_name}()")

    return components


def parse_ocaml(content: str) -> List[str]:
    components = []

    type_pattern = r"type\s+(\w+)"
    type_matches = re.findall(type_pattern, content, re.MULTILINE)
    components.extend([f"type {type_match}" for type_match in type_matches])

    class_pattern = r"class\s+(\w+)\s+=\s+object\s+((.|\n)*?)\s+end"
    class_matches = re.findall(class_pattern, content, re.DOTALL)
    for class_match in class_matches:
        class_name = class_match[0]
        class_body = class_match[1]
        components.append(f"class {class_name}")

        # Match method definitions in the class body
        method_pattern = r"method\s+(\w+)"
        method_matches = re.findall(method_pattern, class_body, re.MULTILINE)
        components.extend(
            [
                f"class {class_name} -> method {method_match}"
                for method_match in method_matches
            ]
        )

    function_pattern = r"let\s+(\w+)\s+\(\)\s+="
    function_matches = re.findall(function_pattern, content, re.MULTILINE)
    components.extend(
        [f"let {function_match} ()" for function_match in function_matches]
    )

    return components


def parse_apl(content: str) -> List[str]:
    components = []

    namespace_pattern = r":Namespace\s+(\w+)"
    namespace_matches = re.findall(namespace_pattern, content, re.MULTILINE)
    components.extend(
        [f":Namespace {namespace_match}" for namespace_match in namespace_matches]
    )

    # Modified pattern to capture both function and string assignment
    assignment_pattern = r"(\w+)\s+←\s+({[^}]*}|'.*?')"
    assignment_matches = re.findall(assignment_pattern, content, re.MULTILINE)
    components.extend(
        [
            f":Namespace {namespace_matches[0]} -> {assignment_match[0]} ← {assignment_match[1]}"
            for assignment_match in assignment_matches
        ]
    )

    return components


def parse_perl(content: str) -> List[str]:
    # Regular expression to find package name and subroutine names
    package_regex = r"^package\s+([\w\d_]+);"
    subroutine_regex = r"^sub\s+([\w\d_]+)"

    # Extract package names
    package_names = [
        "package " + match.group(1)
        for match in re.finditer(package_regex, content, re.MULTILINE)
    ]

    # Extract subroutine names
    subroutine_names = [
        package_names[0] + " -> sub " + match.group(1)
        for match in re.finditer(subroutine_regex, content, re.MULTILINE)
    ]

    return package_names + subroutine_names


def parse_php(content: str) -> List[str]:
    # Regular expression to find class names and their bodies, function names
    class_regex = r"(class\s+([\w\d_]+)\s*{([^}]*)})"
    function_regex = r"function\s+([\w\d_]+)"

    components = []

    # Extract classes and their methods
    for match in re.finditer(class_regex, content, re.MULTILINE):
        start_pos = match.start()
        full_class, class_name, class_body = match.groups()
        components.append((start_pos, f"class {class_name}"))

        # Find methods within the class body
        method_names = re.findall(function_regex, class_body)
        components.extend(
            [
                (start_pos, f"class {class_name} -> function {method_name}")
                for method_name in method_names
            ]
        )

    # Remove the classes from the content
    content_without_classes = re.sub(class_regex, "", content)

    # Extract standalone functions
    for match in re.finditer(function_regex, content_without_classes, re.MULTILINE):
        start_pos = match.start()
        function_name = match.group(1)
        components.append((start_pos, f"function {function_name}"))

    # Sort the components based on their start position and return the component list
    components.sort()
    return [component for _, component in components]


def parse_powershell(contents: str) -> List[str]:
    # Split the contents into lines
    lines = contents.split("\n")

    # Create an empty result list
    result = []

    # Initialize the current class name to None
    current_class_name = None

    # Iterate over the lines in the contents
    for line in lines:
        # Check if the line matches a function
        function_match = re.match(r"function\s+([\w-]+)\s*\((.*?)\)\s*\{", line)
        if function_match:
            result.append(
                "function {}({})".format(
                    function_match.group(1), function_match.group(2)
                )
            )
            continue

        # Check if the line matches a class
        class_match = re.match(r"class\s+([\w]+)\s*\{", line)
        if class_match:
            current_class_name = class_match.group(1)
            result.append("class {}".format(current_class_name))
            continue

        # Check if the line matches a constructor
        constructor_match = re.match(rf"{current_class_name}\s*\((.*?)\)\s*\{{", line)
        if constructor_match and current_class_name:
            arguments = constructor_match.group(1)
            result.append(
                "class {} -> {}({})".format(
                    current_class_name, current_class_name, arguments
                )
            )

        # Check if the line matches a method
        method_match = re.match(r"\s*(?:\[(\w+)\])?\s*([\w-]+)\s*\((.*?)\)\s*\{", line)
        if method_match and current_class_name:
            method_name = method_match.group(2)
            arguments = method_match.group(3)

            if method_match.group(1):  # if return type is present
                return_type = method_match.group(1)
                result.append(
                    "class {} -> [{}]{}({})".format(
                        current_class_name, return_type, method_name, arguments
                    )
                )
            else:  # if return type is not present
                result.append(
                    "class {} -> {}({})".format(
                        current_class_name, method_name, arguments
                    )
                )

        # Check if the line matches a function with a class type argument
        function_with_class_type_arg_match = re.match(
            r"function\s+([\w-]+)\s*\(\[([\w]+)\]\s*([\w-]+)\)\s*\{", line
        )
        if function_with_class_type_arg_match:
            result.append(
                "function {}([{}] {})".format(
                    function_with_class_type_arg_match.group(1),
                    function_with_class_type_arg_match.group(2),
                    function_with_class_type_arg_match.group(3),
                )
            )
            continue

    return result


def parse_matlab(content: str) -> List[str]:
    components = []

    class_pattern = r"(classdef\s+\w+.*?end)"
    class_matches = re.findall(class_pattern, content, re.DOTALL)
    remaining_content = content

    for class_match in class_matches:
        class_content = class_match
        remaining_content = remaining_content.replace(class_content, "")

        class_name_pattern = r"classdef\s+(\w+)"
        class_name = re.search(class_name_pattern, class_content, re.DOTALL).group(1)

        method_pattern = r"methods.*?function\s+(\w+)\("
        method_matches = re.findall(method_pattern, class_content, re.DOTALL)

        for method_match in method_matches:
            components.append(f"classdef {class_name} -> function {method_match}")

    function_pattern = r"function\s+(\w+)\("
    function_matches = re.findall(function_pattern, remaining_content, re.DOTALL)

    for function_match in function_matches:
        components.append(f"function {function_match}")

    return components


def parse_js(content: str) -> List[str]:
    func_pattern = r"\bfunction\s+(\w+)\s*\("
    class_pattern = r"\bclass\s+(\w+)"
    arrow_func_pattern = r"(\w+)\s*=\s*\([^)]*\)\s*=>"
    type_pattern = r"\btype\s+(\w+)\s*="
    funcs = ["function " + n for n in re.findall(func_pattern, content)]
    classes = ["class " + n for n in re.findall(class_pattern, content)]
    arrow_funcs = ["function " + n for n in re.findall(arrow_func_pattern, content)]
    types = ["type " + n for n in re.findall(type_pattern, content)]
    return types + classes + funcs + arrow_funcs


def parse_md(content: str) -> List[str]:
    lines = content.splitlines()
    headers = [line for line in lines if line.startswith("#")]
    return headers


def parse_todo(content: str) -> List[str]:
    todo_pattern = re.compile(r".*TODO: (.*)")
    todos = []

    lines = content.splitlines()
    for line_num, line in enumerate(lines, start=1):
        match = todo_pattern.search(line)
        if match:
            todos.append(f"TODO (Line {line_num}): {match.group(1).strip()}")
    return todos
