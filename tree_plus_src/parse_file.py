# tree_plus_src/parse_file.py
from typing import List, Tuple
import collections
import builtins
import sqlite3
import typing
import yaml
import ast
import os
import re
import json

from tree_plus_src.default_ignore import is_binary

# TODO: convert this to an environment variable and share across the modules
DEBUG = 0


def debug_print(*args, **kwargs):
    if DEBUG:
        print(*args, **kwargs)


def parse_file(file_path: str) -> List[str]:
    """
    Parse a file and return a List[str] of its major components.
    """
    base_file_path, file_extension = os.path.splitext(file_path)
    file_name = os.path.basename(base_file_path)
    file_extension = file_extension.lower()
    # print(f"DEBUG(parse_file): {file_path=}, {file_extension=}\n{path_parts=}")
    components = []

    # handle sqlite databases before trying to open the file
    if file_extension == ".db" or file_extension == ".sqlite":
        components = parse_db(file_path)
        return components

    # skip binary files
    if is_binary(file_path):
        return components

    try:
        with open(file_path, "r", encoding="utf-8") as file:
            contents = file.read()
    except UnicodeDecodeError as ude:
        print(f"tree_plus.parse_file UnicodeDecodeError @ {file_path}: {ude}")
        return components
    except Exception as e:
        print(f"tree_plus.parse_file Exception @ {file_path}: {e}")
        return components

    if file_extension == ".py":
        components = parse_py(contents)
        if isinstance(components, SyntaxError):
            # print(f"SyntaxError @ {file_path}: {components}")
            components = [f"SyntaxError: {components}"]
    elif file_extension == ".json":
        if "package.json" in file_path.lower():
            components = parse_package_json(contents)
        elif "$schema" in contents:  # not great!
            components = parse_json_schema(contents)
    elif file_extension in [".js", ".jsx"]:
        components = parse_js(contents)
    elif file_path.endswith(".d.ts"):
        components = parse_d_dot_ts(contents)
    elif file_extension in {".ts", ".tsx"}:
        components = parse_ts(contents)
        debug_print(f"{file_name=}")
        if "app-routing.module" in file_path:
            components = parse_angular_routes(contents) + components
        elif "app.module" in file_path:
            components = parse_angular_app_module(contents) + components
        elif file_path.endswith("component.ts"):
            components = components + parse_angular_component_ts(contents)
        elif "environment" == file_name or "environment." in file_name:  # paranoid
            components = parse_environment_ts(contents)
        elif "spec.ts" in file_path:
            components = parse_angular_spec(contents) + components
    elif file_extension == ".md":
        components = parse_md(contents)
    elif file_extension == ".env":
        components = parse_dot_env(contents)
    elif file_extension == ".sql":
        components = parse_sql(contents)
    elif file_extension == ".txt":
        if "requirements" in file_name:
            components = parse_requirements_txt(contents)
        else:
            components = parse_txt(contents)
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
    elif file_extension == ".scala":
        components = parse_scala(contents)
    elif file_extension == ".c":
        components = parse_c(contents)
    elif file_extension in {".cpp", ".cc"}:
        components = parse_cpp(contents)
    elif file_extension == ".rs":
        components = parse_rs(contents)
    elif file_extension == ".tf":
        components = parse_tf(contents)
    elif file_extension in (".yml", ".yaml"):
        components = parse_yml(contents)
    elif file_name == "Makefile":
        components = parse_makefile(contents)

    bugs_todos_and_notes = parse_markers(contents)
    total_components = bugs_todos_and_notes + components
    return total_components


def parse_cpp(contents) -> List[str]:
    debug_print("parse_cpp")

    # Combined regex pattern to match all components
    combined_pattern = re.compile(
        # class or struct
        r"\n((class|struct)? \w+( : \w+( \w+)?)?)|"
        # enums (maybe class)
        r"\n((enum) (class )?\w+( : \w+)?)|"
        # (maybe template) functions
        # r"\n((template<.*?>)?\n(\w+::)?(\w+) \w+\([\s\S]*?\))",
        r"((template<.*?>)?\n(\w+::)?(\w+)\s+\w+\([\s\S]*?\))",
        re.DOTALL,
    )

    components = []

    for match in combined_pattern.finditer(contents):
        component = match.group().strip()
        components.append(component)

    return components


# (declare|export) (default)?(\w+ \w+(<.*>)?(\(.*\))?(: \w+)?)
def parse_d_dot_ts(contents) -> List[str]:
    debug_print(contents)
    pattern = r"(declare|export) (default)?(\w+ \w+(<.*>)?(\(.*\))?(: \w+)?)"
    keepers = []
    if matches := re.findall(pattern, contents):
        for matched in matches:
            debug_print(matched)
    return keepers


def parse_angular_app_module(contents) -> List[str]:
    debug_print("parse_angular_app_module")
    pattern = r"(@NgModule\({\n\s*declarations: \[((\n\s*)[a-zA-Z]*,?)*)\n"
    if matches := re.search(pattern, contents, re.DOTALL):
        debug_print(f"{matches=}")
        return [matches[1]]
    return []


def parse_angular_component_ts(contents) -> List[str]:
    debug_print("parse_angular_component_ts")
    keepers = []
    title_pattern = r"title(: string)? = [\"|'](?P<title>.*)[\"|']"
    title_leaf = "    title: string = '?'"
    if title_match := re.search(title_pattern, contents):
        debug_print(f"{title_match.groups()=}")
        title_str = title_match.group("title")
        title_leaf = f"    title: string = '{title_str}'"
    keepers.append(title_leaf)
    variable_pattern = r"[^(private|,)]\s(\w+\??:\s\w+)"
    if variable_matches := re.findall(variable_pattern, contents, re.DOTALL):
        debug_print(f"{variable_matches=}")
        keepers.extend([f"    {v}" for v in variable_matches])
    method_pattern = r"\n +((async)?\s+(?P<name>\w+)\((?P<args>(.|\s)*?)\))"
    if method_matches := re.findall(method_pattern, contents, re.DOTALL):
        for method_match in method_matches:
            method_leaf = method_match[0].strip()
            if "\n" in method_leaf:
                method_leaf_lines = method_leaf.splitlines()
                n = len(method_leaf_lines) - 1
                method_leaf = "\n".join(
                    [
                        f"{'    ' if (i == 0 or i == n) else '        '}{method_leaf_line.strip()}"
                        for i, method_leaf_line in enumerate(method_leaf_lines)
                    ]
                )
            else:
                method_leaf = f"    {method_leaf}"
            keepers.append(method_leaf)
    return keepers


def parse_angular_routes(content) -> List[str]:
    routes_pattern = r"(const routes: Routes = \[\n(?:.|\n)*?\];)"
    routes_match = re.findall(routes_pattern, content)
    if routes_match:
        debug_print(f"{routes_match=}")
        return [routes_match[0]]
    return []


def parse_angular_spec(content) -> List[str]:
    describe_pattern = re.compile(r"(\t*| *)describe\('(.*)'")
    it_pattern = re.compile(r"(\t*| *)it\(('|\")(.*)('|\")")
    components = []
    for line in content.splitlines():
        if match := describe_pattern.search(line):
            debug_print(f"{match.group()}")
            debug_print(f"{match.groups()=}")
            component = f"{match.group(1)}describe '{match.group(2)}'"
        elif match := it_pattern.search(line):
            debug_print(f"{match.group()}")
            debug_print(f"{match.groups()=}")
            debug_print(f"{match.group(3)=}")
            statement = match.group(3).replace("\\", "").replace('"', "'")
            debug_print(f"{statement=}")
            component = f"{match.group(1)}it {statement}"
        else:
            continue
        debug_print(f"{component=}")
        components.append(component)
    return components


def parse_environment_ts(contents) -> List[str]:
    debug_print("parse_environment_ts")
    pattern = re.compile(r"(?P<key>\w+):")
    lines = contents.splitlines()
    parsing = False
    keepers = []
    for line in lines:
        stripped_line = line.strip()
        if stripped_line.startswith("export const environment = {"):
            parsing = True
            continue
        if stripped_line.startswith("};"):
            parsing = False
            break
        if (
            stripped_line.startswith("//")
            or stripped_line.startswith("/*")
            or stripped_line.startswith("*")
        ):
            continue
        if parsing:
            if match := pattern.search(line):
                debug_print(f"{match}")
                keepers.append(f"   {match.group('key')}")
    if keepers:
        keepers = ["environment:"] + keepers
    return keepers


def parse_dot_env(contents) -> List[str]:
    debug_print("parse_dot_env")
    keepers = []
    key_pattern = re.compile(r"([A-Z|_]*)=")
    for line in contents.splitlines():
        if line.startswith("#"):
            continue
        if match := key_pattern.search(line):
            debug_print(match.groups())
            keepers.append(match.group(1))
    return keepers


def parse_requirements_txt(contents) -> List[str]:
    debug_print(f"parse_requirements_txt")
    return [line for line in contents.splitlines() if not line.startswith("#")]


def parse_json_schema(contents) -> List[str]:
    debug_print("parse_json_schema")
    parsed = json.loads(contents)
    keepers = []
    debug_print(parsed)
    for key in ["$schema", "type", "title", "description"]:
        if key in parsed:
            keepers.append(f"{key}: {parsed[key]}")
    return keepers


def parse_package_json(contents) -> List[str]:
    debug_print("parse_package_json")
    keepers = []
    package_json_dict = json.loads(contents)
    debug_print(f"{package_json_dict=}")
    package_json_name = "name: ?"
    if "name" in package_json_dict:
        package_json_name = f"name: '{package_json_dict['name']}'"
    keepers.append(package_json_name)
    package_json_version = "version: ?"
    if "version" in package_json_dict:
        package_json_version = f"version: {package_json_dict['version']}"
    keepers.append(package_json_version)
    package_json_scripts = []
    if "scripts" in package_json_dict:
        package_json_scripts_dict = package_json_dict["scripts"]
        for key, value in package_json_scripts_dict.items():
            package_json_script = f"    {key}: '{value}'"
            package_json_scripts.append(package_json_script)
    if package_json_scripts:
        keepers.append(f"scripts:")
        keepers.extend(package_json_scripts)
    return keepers


# same as js for now
# (path: '(.*)',( |\n).*component: (.*)) }
def parse_ts(content: str) -> List[str]:
    func_pattern = r"\bfunction\s+(\w+)\s*\("
    class_pattern = r"\bclass\s+(\w+)"
    arrow_func_pattern = r"(\w+)\s*=\s*\([^)]*\)\s*=>"
    type_pattern = r"\btype\s+(\w+)\s*="
    funcs = ["function " + n for n in re.findall(func_pattern, content)]
    classes = ["class " + n for n in re.findall(class_pattern, content)]
    arrow_funcs = ["function " + n for n in re.findall(arrow_func_pattern, content)]
    types = ["type " + n for n in re.findall(type_pattern, content)]
    return types + classes + funcs + arrow_funcs


def parse_makefile(contents: str) -> List[str]:
    lines = contents.split("\n")
    commands = [
        line.strip().rstrip(":")
        for line in lines
        if line.strip()
        and not line.startswith("\t")
        and not line.strip().startswith("#")
        and (
            line.startswith(".PHONY")
            or ":" in line
            or line.startswith("include")
            or line.startswith("define")
        )
    ]
    return commands


def parse_sql(contents: str) -> List[str]:
    # Pattern to find the "CREATE TABLE" statements
    pattern_create_table = re.compile(r"CREATE TABLE (\w+) \((.*?)\);", re.DOTALL)
    # List to store the final output
    output = []
    # Find all "CREATE TABLE" statements
    create_table_statements = pattern_create_table.findall(contents)
    # For each "CREATE TABLE" statement
    for table_name, table_body in create_table_statements:
        output.append(f"CREATE TABLE {table_name}")
        # Split the table body into lines and strip leading/trailing spaces
        lines = table_body.strip().split("\n")
        lines = [line.strip() for line in lines]
        # Add each line to the output, with an indent
        output.extend([f"   {line}" for line in lines])
    return output


def is_k8s_yml(contents: str) -> bool:
    for doc in yaml.safe_load_all(contents):
        if "apiVersion" in doc and "kind" in doc and "metadata" in doc:
            return True
    return False


def is_ansible_yml(contents: str) -> bool:
    try:
        docs = list(yaml.safe_load_all(contents))
        if isinstance(docs[0], list) and any(
            isinstance(item, dict) and "name" in item for item in docs[0]
        ):
            return True
    except Exception as e:
        print("Exception in is_ansible_yml: ", e)
        return False
    return False


def is_github_yml(contents: str) -> bool:
    try:
        doc = yaml.safe_load(contents)  # load YAML from string
        if "name" in doc and "jobs" in doc:
            return True
    except Exception as e:
        print("Exception in is_github_yml: ", e)
    return False


def parse_github_yml(contents: str) -> List[str]:
    result = []
    try:
        doc = yaml.safe_load(contents)  # load YAML from string
        if is_github_yml(contents):
            result.append(doc["name"])
            for job in doc["jobs"]:
                result.append(f"  job: {job}")
                if "steps" in doc["jobs"][job]:
                    for step in doc["jobs"][job]["steps"]:
                        if "name" in step:
                            result.append(f"    - {step['name']}")
    except Exception as e:
        print("Exception in parse_github_yml: ", e)
    return result


def parse_k8s(contents: str) -> List[str]:
    result = []
    for doc in yaml.safe_load_all(contents):
        result.append(f"{doc['apiVersion']}.{doc['kind']} -> {doc['metadata']['name']}")
    return result


def parse_ansible(contents: str) -> List[str]:
    result = []
    for doc in yaml.safe_load_all(contents):
        result.extend(item.get("name", "") for item in doc if isinstance(item, dict))
    return result


def parse_yml(contents: str) -> List[str]:
    if is_k8s_yml(contents):
        return parse_k8s(contents)
    elif is_ansible_yml(contents):
        return parse_ansible(contents)
    elif is_github_yml(contents):
        return parse_github_yml(contents)
    return ["Unsupported YAML Category"]


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
    try:
        node = ast.parse(content)
    except SyntaxError as se:
        return se
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


def parse_db(db_path: str) -> List[str]:
    # Connect to the SQLite database
    conn = sqlite3.connect(db_path)
    # Create a cursor object
    cursor = conn.cursor()
    # Execute the query that retrieves all table names
    cursor.execute("SELECT name FROM sqlite_master WHERE type='table';")
    # Fetch all results of the query
    tables = cursor.fetchall()
    # Close the connection
    conn.close()
    # Extract the table names from the tuples and return them
    return [f"CREATE TABLE {table[0]}" for table in tables]


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


def parse_scala(contents: str) -> List[str]:
    lines = contents.split("\n")
    result = []
    current_scope = None
    scope_type = None

    brace_count = 0  # Count braces to keep track of scope

    for line in lines:
        line = line.strip()

        function_match = re.match(r"def (\w+)\((.*)\): (\w+)", line)
        trait_match = re.match(r"trait (\w+)", line)
        trait_func_match = re.match(r"def (\w+): (\w+) =", line)
        object_match = re.match(r"object (\w+)", line)
        case_class_match = re.match(r"case class (\w+)\((.*)\)", line)
        opening_brace_match = re.search(r"\{", line)
        closing_brace_match = re.search(r"\}", line)

        # Increase or decrease brace_count based on opening or closing braces
        if opening_brace_match:
            brace_count += 1
        if closing_brace_match:
            brace_count -= 1

        if function_match:
            function_name, params, return_type = function_match.groups()
            if current_scope:
                result.append(
                    f"{scope_type} {current_scope} -> def {function_name}({params}): {return_type}"
                )
            else:
                result.append(f"def {function_name}({params}): {return_type}")
        elif trait_match:
            trait_name = trait_match.group(1)
            result.append(f"trait {trait_name}")
            current_scope = trait_name
            scope_type = "trait"
        elif trait_func_match and current_scope:
            func_name, return_type = trait_func_match.groups()
            result.append(
                f"{scope_type} {current_scope} -> def {func_name}: {return_type}"
            )
        elif object_match:
            object_name = object_match.group(1)
            result.append(f"object {object_name}")
            current_scope = object_name
            scope_type = "object"
        elif case_class_match:
            case_class_name, params = case_class_match.groups()
            result.append(f"case class {case_class_name}({params})")
            current_scope = None
            scope_type = None

        # If brace_count reaches 0, we are outside of current scope
        if brace_count == 0:
            current_scope = None
            scope_type = None

    return result


def parse_c(content: str) -> List[str]:
    # Define the regular expressions for function, struct, enum, and typedef
    # regex_function = r"((?:\w+\s+)+\*?\s*\w+\s*\([^)]*\)\s*\{[^}]*\})"
    regex_function = r"((?:[\w*]+\s*)+\*?\s*\w+\s*\([^)]*\)\s*\{[^}]*\})"
    regex_struct = r"(struct\s+\w+\s*\{[^}]*\})"
    regex_enum = r"(enum\s+\w+\s*\{[^}]*\})"
    regex_typedef = r"(typedef\s+struct\s*\{[^}]*\}\s*\w+;)"

    # Combine all regexes into a single one, each separated by '|'
    regex = f"{regex_function}|{regex_struct}|{regex_enum}|{regex_typedef}"

    # Find all matches
    matches = re.findall(regex, content, re.DOTALL)

    # Initialize the list to hold the parsed elements
    parsed = []

    # Iterate through the matches
    for match in matches:
        # Only one of the groups will be non-empty
        # Iterate through the match groups
        for group in match:
            # Append the non-empty match to the list
            if group:
                # Check if the group is a typedef
                if "typedef" in group:
                    # Extract only the typedef struct name
                    typedef_name = group.split("}")[1].split(";")[0].strip()
                    parsed.append(f"typedef struct {typedef_name}")
                else:
                    # Extract only the first line for each component
                    parsed.append(group.split("{")[0].strip())

    return parsed


def parse_tf(contents: str) -> List[str]:
    pattern = r'(provider|resource|data|variable|output|locals|module)\s+("[^"]*"\s*"[^"]*"|"[^"]*"|\'[^\']*\'|[^\s]*)\s*[{"{]'
    matches = re.findall(pattern, contents, re.MULTILINE)
    return [f"{match[0]} {match[1]}" if match[1] else match[0] for match in matches]


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
    in_code_block = False
    lines = content.splitlines()
    headers_and_tasks = []
    task_pattern = re.compile(r"(-\s*\[ *[xX]?\])\s*(.*)")
    checked_ancestors = []

    # Regex pattern to match URLs in Markdown.
    url_pattern = re.compile(r'\s*\(<a href=".*">.+</a>\)|<a href=".*">.+</a>')
    link_pattern = re.compile(r"\(.*?\)")

    for line in lines:
        # If we encounter a code block (indicated by ```), toggle in_code_block.
        if line.strip().startswith("```"):
            in_code_block = not in_code_block
        # Only process lines that are not within a code block.
        elif not in_code_block:
            # If the line is a header, add it to the output list.
            if line.startswith("#"):
                line = line.lstrip()
                # Remove URLs from headers.
                clean_header = url_pattern.sub("", line)
                clean_header = link_pattern.sub("", clean_header)
                clean_header = clean_header.strip()
                # Skip headers that consist only of '#' and ' '.
                if not clean_header.lstrip("#").strip():
                    continue
                headers_and_tasks.append(clean_header)
            # If the line is a task, process it accordingly.
            elif task_pattern.match(line.lstrip()):
                indent_level = len(line) - len(line.lstrip())
                task_match = task_pattern.match(line.lstrip())
                task_text = task_match.group(2)
                is_checked = "[x]" in line or "[X]" in line
                # print(f"Checked: {is_checked} Line: {line}")
                task = (
                    indent_level * " "
                    + ("- [x] " if is_checked else "- [ ] ")
                    + task_text
                )
                # For every task, we first remove ancestors that are not parents of the current task
                # This is identified by comparing their indentation level.
                checked_ancestors = [
                    a for a in checked_ancestors if len(a[0]) < len(task)
                ]
                # If the task is checked, we add it to the list of ancestors but don't add to the final output yet.
                # The second element of the tuple is a flag to indicate whether this ancestor should be included in the output.
                if is_checked:
                    ancestor_tuple = (task, False)
                    # print(f"ADD ANCESTOR: {ancestor_tuple=}")
                    checked_ancestors.append(ancestor_tuple)
                # If the task is not checked, we update the flag for all ancestors as they should be included in the final output
                # Then add these ancestors to the output and finally add the current task.
                else:
                    checked_ancestors = [(a[0], True) for a in checked_ancestors]
                    ancestors_to_add = [a[0] for a in checked_ancestors if a[1]]
                    # print(f"ADD ANCESTOR(S) {ancestors_to_add=}")
                    headers_and_tasks.extend(ancestors_to_add)
                    # print(f"ADD TASK {task}")
                    headers_and_tasks.append(task)

    return headers_and_tasks


def parse_txt(content: str) -> List[str]:
    # Update the regex pattern to exclude lines with checked boxes.
    # The [\s*] will match spaces or '*', but not 'x' or 'X'.
    checkbox_pattern = r"-\s*\[\s*([^Xx])?\s*\]\s*(.+)"

    lines = content.split("\n")
    parsed_checkboxes = []

    for line in lines:
        match = re.search(checkbox_pattern, line)
        # If there is a match, normalize the checkbox. If not, print a message for debugging.
        if match:
            # print(f"Matched line: {line}")
            normalized_checkbox = (
                "- [ ] " + match.group(2).strip()
            )  # Use the second group to get checkbox content.
            parsed_checkboxes.append(normalized_checkbox)
        # else:
        #   print(f"Did not match line: {line}")

    return parsed_checkboxes


def parse_markers(content: str) -> List[str]:
    marker_pattern = re.compile(r'(BUG|TODO|NOTE)(?![\'"]): (.*)')
    markers = []
    lines = content.splitlines()
    for line_n, line in enumerate(lines, start=1):
        match = marker_pattern.search(line)
        if match:
            marker, content = match.groups()
            mark = f"{marker.upper()} (Line {line_n}): {content.strip()}"
            markers.append(mark)
    return markers


def parse_rs(contents: str) -> List[str]:
    debug_print("parse_rs")

    # Combined regex pattern to match all components
    combined_pattern = re.compile(
        r"(pub\s+)?(fn\s+[a-z_][a-z_0-9]*\s*(<[^>]+>)?\s*\([^)]*\)\s*(->\s*[^{]*)?(where\s*[^{]*)?|"
        r"struct\s+[A-Z]\w*|impl\s+([A-Z]\w*)(\s+for\s+[A-Z]\w*)?|"
        r"trait\s+[A-Z]\w*|enum\s+[A-Z]\w*|mod\s+[a-z_][a-z_0-9]*|"
        r"macro_rules!\s+[a-z_][a-z_0-9]*)",
        re.DOTALL,
    )

    components = []

    for match in combined_pattern.finditer(contents):
        component = match.group().strip()
        components.append(component)

    return components
