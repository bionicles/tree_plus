# src/parse_file.py
from typing import List
import collections
import builtins
import typing
import ast
import os
import re


def parse_file(file_path: str) -> List[str]:
    """
    Parse a file and return a list of its major components.
    Only supports Python, JavaScript, TypeScript, and Markdown files.
        - Python components: class, method, function, type
        - JavaScript/TypeScript components: class, function, type
        - Markdown components: heading, subheading
    """
    file_extension = os.path.splitext(file_path)[-1].lower()
    components = []

    with open(file_path, "r") as file:
        contents = file.read()

        if file_extension == ".py":
            components = parse_py(contents)

        elif file_extension in [".js", ".ts"]:
            components = parse_js(contents)

        elif file_extension == ".md":
            components = parse_md(contents)

    return components


def parse_py(content: str) -> List[str]:
    def extract_nodes(node, node_type):
        result = []
        if isinstance(node, node_type):
            result.append(node)
        for child in ast.iter_child_nodes(node):
            result.extend(extract_nodes(child, node_type))
        return result

    node = ast.parse(content)
    classes = extract_nodes(node, ast.ClassDef)
    types = [
        n.targets[0].id
        for n in extract_nodes(node, ast.Assign)
        if isinstance(n.value, ast.Call)
        and (is_typing_construct(n.value.func) or is_builtin_type(n.value.func))
    ]
    classnames = ["class " + n.name for n in classes]
    methods = [
        f"class {n.name} -> def {m.name}"
        for n in classes
        for m in extract_nodes(n, ast.FunctionDef)
    ]
    funcs = extract_nodes(node, ast.FunctionDef)
    funcnames = [
        "def " + n.name
        for n in funcs
        if not any(n.name == m.split()[-1] for m in methods)
    ]

    return types + classnames + methods + funcnames


def is_typing_construct(node):
    if isinstance(node, ast.Name):
        return node.id in typing.__all__
    elif isinstance(node, ast.Attribute) and node.value.id == "typing":
        return node.attr in typing.__all__
    return False


def is_builtin_type(node):
    if isinstance(node, ast.Name):
        return node.id in dir(builtins) + dir(collections)
    return False


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
