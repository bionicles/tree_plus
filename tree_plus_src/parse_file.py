# tree_plus_src/parse_file.py
from functools import lru_cache
from typing import Dict, List, Sequence, Tuple, Optional, Union
from pathlib import Path
import json
import os
import re

from tree_plus_src.debug import debug_print

BINARY_CHECK_SIZE = 1024
TEXTCHARS = bytearray({7, 8, 9, 10, 12, 13, 27} | set(range(0x20, 0x100)) - {0x7F})
LISP_EXTENSIONS = {".lisp", ".clj", ".scm", ".el", ".rkt"}
JS_EXTENSIONS = {".js", ".jsx", ".ts", ".tsx"}
C_EXTENSIONS = {".c", ".cpp", ".cc", ".h", ".cu", ".cuh"}
COBOL_EXTENSIONS = {".cbl", ".cobol"}
FORTRAN_EXTENSIONS = {
    ".f",
    ".for",
    ".f77",
    ".f90",
    ".f95",
    ".f03",
    ".f08",
    ".F",
    ".F90",
}
MATHEMATICA_EXTENSIONS = {".nb", ".wl"}
PYTHON_EXTENSIONS = {".py", ".pyi"}


@lru_cache(maxsize=None)
def read_file(
    file_path: str,
    raise_exceptions: bool = False,
    n_lines: Optional[int] = None,
) -> str:
    """
    Read the content of a file and return it as a string.
    Optionally raise exceptions if encountered.

    :param file_path: Path to the file to be read.
    :param raise_exceptions: Flag to control exception handling.
    :return: Content of the file as a string.
    """
    try:
        with open(file_path, "r", encoding="utf-8") as file:
            if n_lines is None:
                contents = file.read()
            else:
                contents = "\n".join([next(file) for _ in range(n_lines)])
        return contents
    except Exception as e:
        if raise_exceptions:
            raise
        else:
            debug_print(f"read_file Exception @ {file_path}: {e}")
            return ""


def parse_file(
    file_path: Union[str, Path],
    contents: Optional[str] = None,
) -> List[str]:
    "Parse a file and return a List[str] of its major components."
    # always convert to str since this was built on 'os', not pathlib
    debug_print(f"parse_file: {file_path=}")
    if isinstance(file_path, Path):
        file_path = str(file_path)
    base_file_path, file_extension = os.path.splitext(file_path)
    file_name = os.path.basename(base_file_path)
    file_extension = file_extension.lower()
    debug_print(f"parse_file: {file_path=} {file_name=} {file_extension=}")
    components = []

    # handle sqlite databases before trying to open the file
    if file_extension == ".db" or file_extension == ".sqlite":
        return parse_db(file_path)

    # skip reading binary files
    if not contents and is_binary(file_path):
        return components

    n_lines = None
    if file_extension == ".csv":
        n_lines = 3

    if contents is None:
        debug_print("reading file because no contents given")
        contents = read_file(file_path, n_lines=n_lines)
    else:
        debug_print(contents)
        contents = contents

    # just once
    in_tensorflow = "tensorflow" in file_path

    if file_extension in JS_EXTENSIONS:
        if file_path.endswith(".d.ts"):
            components = parse_d_dot_ts(contents)
        else:
            components = parse_ts(contents)
            # angular here
            if "spec.ts" in file_path:
                components = parse_angular_spec(contents)
            if "app-routing.module" in file_path:
                components = parse_angular_routes(contents) + components
            elif "app.module" in file_path:
                components = parse_angular_app_module(contents) + components
            elif "environment" == file_name or "environment." in file_name:  # paranoid
                components = parse_environment_ts(contents)
    elif file_extension in PYTHON_EXTENSIONS:
        components = parse_py(contents)
        if isinstance(components, SyntaxError):
            components = [f"SyntaxError: {components}"]
    elif file_extension == ".md":
        components = parse_md(contents)
    elif file_extension == ".rst":
        components = parse_rst(contents)
    elif file_extension == ".json":
        if "package.json" in file_path.lower():
            components = parse_package_json(contents)
        elif "$schema" in contents:  # not great!
            components = parse_json_schema(contents)
        elif 'jsonrpc": "2' in contents:
            components = parse_json_rpc(contents)
        elif 'openrpc": "' in contents:
            components = parse_openrpc_json(contents)
    elif file_extension in (".yml", ".yaml"):
        components = parse_yml(contents)
    # elif file_extension == ".c":
    #     components = parse_c(contents)
    # components = func_timeout(1, parse_c, (contents,))
    elif file_extension in C_EXTENSIONS:
        # special case: handling flags
        if in_tensorflow and file_extension in (".cc", ".h") and "flags" in file_name:
            components = parse_tensorflow_flags(contents)
            more_components = parse_c(contents)
            components.extend(more_components)
        else:
            components = parse_c(contents)
    elif file_extension == ".php":
        components = parse_php(contents)
    elif file_extension == ".rs":
        components = parse_rs(contents)
    elif file_extension == ".kt":
        components = parse_kt(contents)
    elif file_extension == ".swift":
        components = parse_swift(contents)
    elif file_extension == ".go":
        components = parse_go(contents)
    elif file_extension == ".sh":
        components = parse_bash(contents)
    elif file_extension == ".ps1":
        components = parse_ps1(contents)
    elif file_extension == ".zig":
        components = parse_zig(contents)
    elif file_extension == ".rb":
        components = parse_rb(contents)
    elif file_name == "Makefile":
        components = parse_makefile(contents)
    elif file_extension == ".sql":
        components = parse_sql(contents)
    elif file_extension == ".env" or file_name.startswith(".env"):
        components = parse_dot_env(contents)
    elif file_extension == ".txt":
        if "requirements" in file_name:
            components = parse_requirements_txt(contents)
        else:
            components = parse_txt(contents)
    elif file_extension == ".graphql":
        components = parse_graphql(contents)
    elif file_extension == ".cs":
        components = parse_cs(contents)
    elif file_extension == ".jl":
        components = parse_jl(contents)
    elif file_extension == ".scala":
        components = parse_scala(contents)
    elif file_extension == ".java":
        components = parse_java(contents)
    elif file_path.endswith("Cargo.toml"):
        components = parse_cargo_toml(contents)
    elif file_path.endswith("pyproject.toml"):
        components = parse_pyproject_toml(contents)
    elif file_extension == ".csv":
        components = parse_csv(contents)
    elif file_extension == ".pl":
        components = parse_perl(contents)
    elif file_extension == ".hs":
        components = parse_hs(contents)
    elif file_extension == ".fs":
        components = parse_fsharp(contents)
    elif file_extension in LISP_EXTENSIONS:
        components = parse_lisp(contents)
    elif file_extension in {".erl", ".hrl"}:
        components = parse_erl(contents)
    elif file_extension == ".capnp":
        components = parse_capnp(contents)
    elif file_extension == ".proto":
        components = parse_grpc(contents)
    elif file_extension == ".tex":
        components = parse_tex(contents)
    elif file_extension == ".lean":
        components = parse_lean(contents)
    elif file_extension in FORTRAN_EXTENSIONS:
        components = parse_fortran(contents)
    elif file_extension == ".tf":
        components = parse_tf(contents)
    elif file_extension == ".thy":
        components = parse_isabelle(contents)
    elif file_extension == ".lua":
        components = parse_lua(contents)
    elif file_extension == ".tcl":
        components = parse_tcl(contents)
    elif file_extension == ".m":
        if "@interface" in contents or "@implementation" in contents:
            components = parse_objective_c(contents)
        elif "classdef" in contents or "methods" in contents:
            components = parse_matlab(contents)
    elif file_extension.lower() == ".r":
        components = parse_r(contents)
    elif file_extension.lower() in MATHEMATICA_EXTENSIONS:
        components = parse_mathematica(contents)
    elif file_extension == ".matlab":
        components = parse_matlab(contents)
    elif file_extension == ".ml":
        components = parse_ocaml(contents)
    elif file_extension.lower() in COBOL_EXTENSIONS:
        components = parse_cbl(contents)
    elif file_extension == ".apl":
        components = parse_apl(contents)
    elif file_extension == ".html":
        components = parse_html(contents)

    bugs_todos_and_notes = parse_markers(contents)
    total_components = bugs_todos_and_notes + components
    return total_components


def extract_groups(match: re.Match, named_only: bool = False) -> dict:
    "filter and debug print non-None match groups"
    numbered_groups = {}
    if match is not None:
        if not named_only:
            for i in range(len(match.groups())):
                group = match.group(i)
                if group:
                    numbered_groups[i] = group
        for k, v in match.groupdict().items():
            if v or k == "blank_line":
                numbered_groups[k] = v
    debug_print("groups:")
    debug_print(numbered_groups)
    return numbered_groups


from bs4 import BeautifulSoup
from rich.panel import Panel


def parse_html(contents: str) -> List[str]:
    return []
    # disabled for now
    # components = components_from_html(contents)
    # return components


# if tag.name == "tr" and len(tag.find_all("tr")) > 1:
#     continue
# component = component.replace("  ", " ").replace("[edit]", "")
#
# if tag.name in "tr":
# component = re.sub(r"^\s{2,}", "", component, flags=re.MULTILINE)
# component = re.sub(r"\n+", " ", component)

# if component.startswith("  "):
#     component = component[2:]


DENY_HTML = "\n"

import re
from bs4 import BeautifulSoup
from rich import print as rprint
from rich.panel import Panel
from rich.markdown import Markdown
from rich.text import Text
from rich.table import Table
from typing import List, Optional, Tuple

tags_allowed = (
    "title",
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    # "ol",
    "table",
    # "tr",
    # "p",
)


# BUG: HTML tree doesn't look awesome (yet)
# TODO: Fix HTML in TreePlus (How?)
def process_tag(tag, components) -> Optional[str]:
    component = (
        tag.get_text(" ")
        .strip("\n")
        .replace("  ", " ")
        .replace(" . ", ". ")
        .replace(" , ", ", ")
    )
    if not component:
        return None
    if component in [c[0] for c in components]:
        return None

    if tag.name == "title":
        return f"# {component}"
    elif tag.name.startswith("h"):
        level = int(tag.name[1])
        replaced_component = component.replace("\n", "")
        return f"\t{'#' * level} {replaced_component}"
    elif tag.name == "a":
        url = tag.get("href")
        return f"[link={url}]{component}[/link]"
    elif tag.name in ("section", "tr", "p", "ol"):
        return prettify_tr(component)
    return component


# , source: Optional[str] = None # customization is possible
def components_from_html(contents: str) -> List[str]:
    soup = BeautifulSoup(contents, "html.parser")
    components = []
    body = soup.body
    if body is None:
        return []

    # BUG: this repeatedly finds tags, need to avoid repeating ourselves
    for tag in body.find_all(tags_allowed):
        component = process_tag(tag, components)
        if component:
            components.append(component)

    return components


def prettify_tr(component: str) -> str:
    lines = component.splitlines()
    output_str = ""
    n = 0
    for line in lines:
        stripped_line = line.strip().replace("  ", " ")
        if not stripped_line:
            continue
        if n == 0:
            formatted_line = f"# {stripped_line}"
        else:
            formatted_line = f"\n\t{n}. {stripped_line}"
        output_str += formatted_line
        n += 1
    return output_str


def hierarchical_numbering(components):
    stack = []
    numbered_components = []
    for i, (component, pretty_component) in enumerate(components):
        level = len(re.findall(r"#", pretty_component)) - 1
        while stack and stack[-1][1] >= level:
            stack.pop()
        if stack:
            prefix = ".".join(str(s[1]) for s in stack) + "."
            numbered_components.append(f"{prefix}{level+1}. {component}")
        else:
            numbered_components.append(f"{level+1}. {component}")
        stack.append((component, level))
    return numbered_components


OOPS = re.compile(r"(?P<screwup>(?P<lower>[a-z])(?P<upper>[A-Z]))")
parser = "html.parser"
# parser = "html5lib"
# from rich.markdown import Markdown
# from rich.text import Text

# tags_allowed = (
#     "title",
#     "h1",
#     "h2",
#     "h3",
#     "h4",
#     "h5",
#     "h6",
#     "ol",
#     # "ul",
#     # "section",
#     "tr",
#     # "p",
# )


# def components_from_html(
#     contents: str,
#     source: Optional[str] = None,
# ) -> List[str]:
#     soup = BeautifulSoup(contents, parser)
#     components = []

#     body = soup.body
#     if body is None:
#         return []

#     for tag in body.find_all(tags_allowed):
#         if tag == body.contents:
#             continue

#         component = (
#             tag.get_text(" ")
#             .strip("\n")
#             .replace("  ", " ")
#             .replace(" . ", ". ")
#             .replace(" , ", ", ")
#         )

#         if not component:
#             continue

#         if component in components:
#             continue

#         debug_print(tag)
#         pretty_component = None

#         if tag.name == "title":
#             pretty_component = f"# {component}"

#         if tag.name in ("h1", "h2", "h3", "h4", "h5", "h6"):
#             n = int(tag.name[1])
#             pretty_component = f"\t{'#' * n} {component.replace("\n", "")}"

#         if "mathcal" in component:
#             pretty_component = Text(component)
#         else:
#             if tag.name in ("section", "tr", "p", "ol"):
#                 if tag.name == "p":
#                     component = component.replace("\n", " ").replace("  ", " ")
#                 pretty_component = prettify_tr(component)

#         pretty_component = pretty_component or component
#         debug_print(Panel(pretty_component, title=tag.name))
#         components.append((component, pretty_component))

#     subbed_components = []
#     for i, (component, pretty_component) in enumerate(components):
#         if isinstance(pretty_component, (Markdown, Text)):
#             subbed_components.append(pretty_component)
#         else:
#             for j, other_component in enumerate(components[i:]):
#                 if isinstance(other_component, str) and other_component in component:
#                     components.pop(i)
#                     break
#             subbed_component = (
#                 component.replace("\n\n", "\n").strip().replace("\r", " ")
#             )
#             subbed_components.append(subbed_component)

#     # return subbed_components
#     return subbed_components


# ONLY_DIGITS = re.compile(r"^(\d\.)+$")


# def prettify_tr(component: str, ul=1) -> str:
#     lines = component.splitlines()
#     output_str = ""
#     n = 0
#     for line in lines:
#         stripped_line = line.strip().strip(" ").replace("  ", " ")
#         if all(c.isdigit() or c in ". \n" for c in stripped_line):
#             continue
#         if len(stripped_line) <= 1:
#             continue
#         if not stripped_line:
#             continue
#         if stripped_line.lower() in output_str.lower():
#             continue
#         if n == 0:
#             formatted_line = f"# {stripped_line}"
#         else:
#             formatted_line = f"\n\t- {n}. {stripped_line}"
#         output_str += formatted_line
#         n += 1
#     return output_str

# output = "".join(output)


# m := OOPS.search(subbed_component):
#             groups = extract_groups(m, named_only=True)
#             # subbed_component = subbed_component.replace(
#             #     groups["screwup"], f"{groups['lower']} !!! {groups['upper']}"
#             # )
# def prettify_ol(component: str) -> str:
#     lines = component.splitlines()
#     output = []
#     n = 0
#     for line in lines:
#         stripped_line = line.strip()
#         if len(stripped_line) <= 1:
#             continue
#         if n == 0:
#             formatted_line = f"# {line}"
#         else:
#             formatted_line = f"\n\t- {n}. {line}"
#         output.append(formatted_line)
#         n += 1
#     output = "".join(output)
#     return output


def assemble_tensorflow_flag(
    flag_type: str, flag: str, description: Optional[List[str]] = None
) -> str:
    flag = f"{flag_type}('{flag}')"
    if description:
        flag += ":\n\t" + " ".join(line.strip() for line in description)
        if not flag.endswith("."):
            flag += "."
    # flag += ")"
    return flag


def parse_tensorflow_flags(contents: str) -> List[str]:
    debug_print("parse_tensorflow_flags")
    pattern = re.compile(
        r"^(?: |\{)+(?P<flag_type>Flag|TF_PY_DECLARE_FLAG|TF_DECLARE_FLAG)\((?:\s*?)\"?(?P<flag_name>\w+)?\"?|"
        r"^\s+\"(?P<flag_description>[\w* \-\/=;><,:+().']+)(?=\.\s?\")?|"
        r"(?P<blank_line>^$)",
        re.MULTILINE,
    )
    components = []
    carry_description = None
    carry_flag_type = None
    carry_flag = None
    for n, match in enumerate(pattern.finditer(contents)):
        debug_print(f"parse_tensorflow_flags {n=} {match=}")
        debug_print(
            f"parse_tensorflow_flags {carry_flag_type=} {carry_flag=} {carry_description=}"
        )
        component = None
        groups = extract_groups(match, named_only=True)
        if "flag_name" in groups:
            if carry_flag_type and carry_flag:
                component = assemble_tensorflow_flag(
                    carry_flag_type, carry_flag, carry_description
                )
                carry_description = None
                carry_flag_type = None
                carry_flag = None
            carry_flag_type = groups["flag_type"]
            carry_flag = groups["flag_name"]
        elif "flag_description" in groups:
            if carry_flag is None:  # skip false positives
                continue
            description = groups["flag_description"]
            if carry_description is None:
                carry_description = [description]
            else:
                carry_description.append(description)
        elif "blank_line" in groups and carry_flag and carry_flag_type:
            # handling the edge case of inadvertent connections
            component = assemble_tensorflow_flag(
                carry_flag_type, carry_flag, carry_description
            )
            carry_description = None
            carry_flag_type = None
            carry_flag = None
        if component:
            debug_print(f"COMPONENT:", component)
            components.append(component)
    return components


def parse_rst(contents: str) -> List[str]:
    debug_print("parse_rst")
    pattern = re.compile(
        r"^(?P<rst_header>(?P<content>.*)$\s(?P<line>(?P<subheading>-+)|(?P<heading>=+))(?=$))",
        re.MULTILINE,
    )

    components = []
    for n, match in enumerate(pattern.finditer(contents)):
        debug_print(f"parse_rst {n=} {match=}")
        groups = extract_groups(match, named_only=True)
        component = None
        if "rst_header" in groups:
            prefix = "# " if "heading" in groups else "- "
            component = prefix + groups["content"]

        if component:
            debug_print(f"parse_rst component:", component)
            components.append(component)

    return components


# r"\n((template<.*?>)?\n(\w+::)?(\w+) \w+\([\s\S]*?\))",
# r"((template<.*?>)?\n(\w+::)?(\w+)\s+\w+\([\s\S]*?\))",
# r"\n(template.*)\n|"
# r"\n(((\w+::)?(\w+))\s+\w+\([\s\S]*?\))",
# r"\n(\b[\w:]+(?:<[^>]*>)?\s+\w+\s*\([^)]*\)\s*)\s*{",
# r"\n(\b[\w:]+(?:<[^>]*>)?\s+\w+\s*\([^)]*\))\s*(?=\{)",

# ^(?P<typedef_struct>typedef\s+struct\s+(?P<name1>\w+)?\s*\{[^}]*\}\s*(?P<name2>\w+)(?=;\s))

# wow:
# r"^(?P<method> +(?P<virtual>virtual )?(?P<method_return_type>\w+ )?~?(?P<method_name>\w+)(?P<args>\(.*?\)[^,](?P<const_override>( const)?( override)?)?)(?P<postfix>(\s^ *)?(?P<colon>\s?:\s).*?)?)(?=(\s{)|(?=.*=.*;\n)|(?=\s^))|"


# refactor with newer regex learning to cover more and avoid catastrophic backtracking
def parse_c(contents: str) -> List[str]:
    debug_print("parse_cpp")
    contents = remove_c_comments(contents)

    # Combined regex pattern to match all components
    combined_pattern = re.compile(
        # Functions first (most common)
        r"^(?P<function> *(?P<modifier>[\w:]+ )?(?P<function_return_type>[\w:*&]+(?P<generics>\s?<[^>]*>\s?)? )(?P<function_name>[\w*&[\]]+)\([^\)]*\)(?=\s{))|"
        # templates
        # r"^(?P<template>template ?<.*?>[^\{^;^=\n]*(?=\s))|"
        r"^(?P<template>(?P<template_body>template ?<.*?>[\s\S]*?)(?P<tail>;|{|\)))|"
        # hashtag macros
        r"^(?P<macro>#(?:define)(?P<invocation>\s\w+( ?\w* ?\(.*\))?)?)|"
        # Methods
        r"(?<!\\\n)^(?P<method> +(?P<virtual>virtual |static )?(?P<method_return_type>\w+ )?~?(?P<method_name>[*\w]+)(?P<args>\(.*\)(?P<const_override>( const)?( override)?)?)(?P<postfix>(\s^ *)?(?P<colon>\s?:\s).*?)?)(?=(\s{)|(?=.*=.*;\n)|(?=\s^))|"
        # typedef struct
        r"^(?P<typedef_struct>typedef struct\s?(?P<type_struct_name1>\w+)?(?P<body>\s\{[\s\S]*?(?P<closing>\} (?P<type_struct_name2>\w+))))|"
        # structs
        r"^(?P<struct>(?:static )?struct(?P<struct_name> \w+)?(?=\s\{))|"
        # typedef struct weirdness with two names
        # class
        r"^(?P<class>class \w+(?P<inheritance>\s?:(?P<mod>\s\w+\s\w+,?)*)?)|"
        # enums (maybe class)
        r"^(?P<enum>(?:enum) (?:class )?\w+(?: : \w+)?)|"
        # public or private sections
        r"^(?P<public_or_private> *(public|private):)|"
        # pybind modules
        r"^(?P<pybind11>PYBIND11_MODULE[\s\S]*?)(?={)|"
        # pybind defs
        r"^(?P<def> *\w+\.def\(\"[\s\S]*?(?=;))|"
        # static definitions seem important
        r"^(?P<other_static>static (struct )?(?P<static_kind>\w+) \w+(\[\])?(?= =))",
        # functions
        re.MULTILINE,
    )

    components = []
    public_or_private = None
    for n, match in enumerate(combined_pattern.finditer(contents)):
        component = None  # just in case!
        debug_print(f"parse_cpp {n=} {match=}")
        groups = extract_groups(match, named_only=True)
        if "function" in groups:
            component = groups["function"]
            public_or_private = None
        elif "method" in groups:
            if public_or_private is not None:
                components.append(public_or_private)
                public_or_private = None
            component = groups["method"].rstrip(";")
        elif "struct" in groups:
            component = groups["struct"]
        elif "typedef_struct" in groups:
            name = (
                groups["type_struct_name1"]
                if "type_struct_name1" in groups
                else (
                    groups["type_struct_name2"] if "type_struct_name2" in groups else ""
                )
            )
            if name:
                component = f"typedef struct {name}"
            else:
                component = None
        elif "public_or_private" in groups:
            # carrying this but not adding it eagerly to avoid scope confusion
            public_or_private = groups["public_or_private"]
            continue
        elif "class" in groups:
            component = groups["class"]
            public_or_private = None  # right?
        elif "enum" in groups:
            component = groups["enum"]
            public_or_private = None  # right?
        elif "template" in groups:
            component = (
                groups["template_body"]
                .rstrip("\n")
                .rstrip(" ")
                .rstrip("\n")
                .rstrip("\n")
            )
            if groups["tail"].startswith(")"):
                component += ")"
            public_or_private = None  # right?
        elif "macro" in groups:
            component = groups["macro"]
            public_or_private = None  # right?
        elif "other_static" in groups:
            component = groups["other_static"]
        elif "pybind11" in groups:
            component = groups["pybind11"]
        elif "def" in groups:
            component = groups["def"]
        if component:
            debug_print(f"{component=}")
            component = component
            component = component.rstrip("\n").rstrip(" ")
            components.append(component)

    return components


@lru_cache
def is_binary_string(data: bytes) -> bool:
    return bool(data.translate(None, TEXTCHARS))


@lru_cache
def is_binary(file_path: str) -> bool:
    """
    Check if a file is binary or not.
    """
    try:
        # read the file in binary mode
        with open(file_path, "rb") as f:
            return is_binary_string(f.read(BINARY_CHECK_SIZE))
    except Exception as e:
        print(f"Error opening file {file_path}: {e}")
        return False


def clean_isabelle_text(content: str) -> str:
    string = content
    debug_print("string", string)
    return string.replace('"', "'").replace("'", '"')


def parse_isabelle(contents: str) -> List[str]:
    from tree_plus_src.isabelle_symbols import replace_isabelle_symbols

    debug_print("parse_isabelle")

    pattern = re.compile(
        # title
        r"^\(\* *(?P<title>Title: *[^\n]+)|"
        r"^ *(?P<author>Author: *[^\n]+)|"
        r"^ *(?P<theory>theory *[^\n]+)|"
        r"^ *(?P<section>(sub)?section *[^\n]+)|"
        r"^ *(?P<class>class *[^\n]+)|"
        r"^ *(?P<definition>(qualified )?definition \w+ :: \"[^\"]*\")|"
        r"^ *(?P<function>(qualified )?fun(ction)? \w+ :: \"[^\"]*\")|"
        r"^(?P<claim>(?P<lemma_or_locale> *(qualified )?(?:theorem|lemma|locale|corollary).*)(\s^ *\".*)?(\s^ *(?:fixes|assumes|shows|obtains).*((\s^ *(?:and|\(is).*(\s^.*(and|\"))*)*)*)*.*)(?=\s^)|"
        r"^(?P<fin>end)",
        re.MULTILINE,
    )

    components = []
    for n, match in enumerate(pattern.finditer(contents)):
        debug_print(f"parse_isabelle {n=} {match=}")
        groups = extract_groups(match, named_only=True)
        component = None
        if "title" in groups:
            component = groups["title"]
        elif "author" in groups:
            component = groups["author"]
        elif "theory" in groups:
            component = groups["theory"]
        elif "section" in groups:
            component = replace_isabelle_symbols(groups["section"])
            component = clean_isabelle_text(component)
        elif "class" in groups:
            component = groups["class"]
        elif "definition" in groups:
            component = replace_isabelle_symbols(groups["definition"])
        elif "function" in groups:
            component = replace_isabelle_symbols(groups["function"])
        elif "claim" in groups:
            component = replace_isabelle_symbols(groups["claim"])
        elif "fin" in groups:
            component = groups["fin"]

        if component:
            debug_print(f"match {n} component:\n{component}")
            components.append(component)

    return components


def parse_fortran(contents: str) -> List[str]:
    debug_print("parse_fortran")

    # remove comments
    contents = re.sub(r" ?! ?[^\"]*?(?=\n)", "", contents)

    # Regex pattern to match Fortran components including multiline subroutine signatures
    combined_pattern = re.compile(
        # Match traditional subroutines (single line and multiline)
        r"^(?P<signature> *SUBROUTINE\s+\w+\([^\)]*\))(?P<details>[\s\S]*?(?P<ending>\s*END\s+SUBROUTINE\s+\w+))(?=\s|$)|"
        # Match types to the end
        r"^(?P<typedef> *TYPE[\s\S]*?(?:END TYPE \w+))|"
        # Match PROGRAM and label start and end
        r"^((?P<programstart>PROGRAM\s+\w+)[\s\S]*?(?P<programend>END PROGRAM \w+))\s?|"
        # Match MODULE without its contents (so we don't consume the subroutines)
        r"^(?P<module>MODULE \w+)|" r"^(?P<endmodule>END MODULE \w+)",
        re.MULTILINE,
    )

    components = []
    for n, match in enumerate(combined_pattern.finditer(contents)):
        debug_print(f"parse_fortran {n=} {match=}")
        component = None
        groups = extract_groups(match)
        if "signature" in groups and "ending" in groups:
            component = groups["signature"] + groups["ending"]
        elif "programstart" in groups and "programend" in groups:
            component = groups["programstart"] + "\n" + groups["programend"]
        elif "typedef" in groups:
            component = groups["typedef"]
        elif "module" in groups:
            component = groups["module"]
        elif "endmodule" in groups:
            component = groups["endmodule"]
        if component:
            debug_print(f"parse_fortran {n=} {component=}")
            components.append(component)
    return components


c_comment_pattern = re.compile(r"(\s)*//.*(\s*,\s*|\s*\))?$|\s*/\*.*?\*/", re.MULTILINE)


def remove_c_comments(contents: str) -> str:
    return re.sub(c_comment_pattern, "", contents)


combined_ts_pattern = re.compile(
    r"^(?P<function> *?(?P<function_export>export (?P<function_default>default )?)?(?P<preamble>(?P<preceding_paren>\(?)|(?P<assignment>(?P<type>const|var|let) \w+ = )|(?P<returns>return )|(?P<key>\w+: )?)(?P<function_async> *?async)? *?(?P<wrapper>\w+\()?function ?\w*(?P<function_generics><.*?>)?(?P<function_args>\([\s\S]*?\))(?P<function_return_type>:\s.*?)?(?= {))|"
    r"^(?P<arrow> *(?P<arrow_export>export (?P<arrow_default>default )?)?(?P<mutability>(const|let|var) )?\w+(?P<colon_or_equals>:|(?: =))(?: async)?[^\(](?P<arrow_args>\((?P<arrow_inner_args>(?P<arrow_positional_args>[\w\s,:?/()>\"|<\'=]*?)|(?P<arrow_destructuring>{[\s\S]*?}(?P<arrow_destructuring_type_hint>:\s{[\s\S]*?})?))\))(?P<arrow_return_hint>: [\w<>]+)? =>)|"
    # r"^(?P<method> +(?P<private>private )?(?P<static>static )?(?P<async>async )?(?P<method_name>\w+)(?P<method_generics><[^>]*?>)?(?P<method_args>\((?P<method_inner_args>(?P<method_positional_args>[\w\s,:?/()>\"|<\'=]*?)|(?P<method_destructuring>{[\s\S]*?(?P<method_destructuring_type_hint>\s{[\s\S]*?})?))\)?)(?P<method_return_hint>:\s[^\{;]*?(?P<where> & { where\??:[\s\S]*(?={\s)))?)?(?P<fin>(?:;| {(?P<space_or_close>{|\s|})))|"
    # 3 failed r"^(?P<method> +(?P<private>private )?(?P<static>static )?(?P<async>async )?(?P<method_name>\w+)(?P<method_generics><[^>]*?>)?(?P<method_args>\((?P<method_inner_args>(?P<method_positional_args>[\w\s,:?/()>\"|<\'=]*?)|(?P<method_destructuring>{[\s\S]*?(?P<method_destructuring_type_hint>\s{[\s\S]*?})?))\))(?P<method_return_hint>:\s[^\{;]*?(?P<where> & { where\??:[\s\S]*(?={\s))?)?)?(?P<fin>(?:;| {(?P<space_or_close>{|\s)))|"
    r"^(?P<method> +(?P<private>private )?(?P<static>static )?(?P<async>async )?(?P<method_name>\w+)(?P<method_generics><[^>]*?>)?(?P<method_args>\((?P<method_inner_args>(?P<method_positional_args>[\w\s,:?/()>\"|<\'=]*?)|(?P<method_destructuring>{[\s\S]*?(?P<method_destructuring_type_hint>\s{[\s\S]*?})?))\))(?P<method_return_hint>:\s[^\{;]*?(?P<where> & { where\??:[\s\S]*(?={\s))?)?)?(?P<fin>(?:;| {(?P<space_or_close>\s|})))|"
    r"^(?P<class_or_interface>(?P<export>export (?:default )?)?(?P<data_type>class|interface)\s+?(?P<structure_name>[\w, ]+?)(?P<class_or_interface_generics><.*>)?(?P<inheritance>\s+?(?:extends|implements)\s+?[\w<>, ]+)?)(?=\s)|"
    r"^(?P<type_definition>type\s+?(?P<type_name>\w+)(?P<generics>[\w<>, ]+?)?)(?=\s=\s)|"
    r"^(?P<jsdoc> +\* +@(?P<tag>category|typedefn|sig|param|returns?)(?P<multiline> {{?[\s\S]*?}?})?.*(?=$))|"
    r"^(?P<jsdoc_untagged> \* \w+.*)$|"
    r"^(?P<object_scope>(?P<object_scope_type>var|const|let) [\w_]+\s+?=\s+?\{)",
    re.MULTILINE,
)


def parse_ts(contents: str) -> List[str]:
    debug_print("parse_ts")
    contents = remove_c_comments(contents)

    components = []
    object_scope = None
    jsdocs = []
    seen_jsdoc = False
    for match_number, match in enumerate(combined_ts_pattern.finditer(contents)):
        debug_print(f"parse_ts: {match_number=} {match=}")
        groups = extract_groups(match, named_only=True)
        component = None
        if "arrow" in groups:
            component = groups["arrow"]
            if object_scope:
                components.append(object_scope)
                object_scope = None
                debug_print(f"parse_ts: SET OBJECT_SCOPE = '{object_scope}'")
        elif "function" in groups:
            component = groups["function"].lstrip("(")
            if object_scope:
                components.append(object_scope)
                object_scope = None
                debug_print(f"parse_ts: SET OBJECT_SCOPE = '{object_scope}'")
        elif "method" in groups:
            component = groups["method"]
        elif "class_or_interface" in groups:
            component = groups["class_or_interface"]
            object_scope = None
            debug_print(f"parse_ts: SET OBJECT_SCOPE = '{object_scope}'")
        elif "type_definition" in groups:
            component = groups["type_definition"]
            object_scope = None
            debug_print(f"parse_ts: SET OBJECT_SCOPE = '{object_scope}'")
        # we carry a scope to append exactly once if it contains one or more functions
        elif "jsdoc" in groups:
            seen_jsdoc = True
            jsdoc = groups["jsdoc"]
            jsdocs.append(jsdoc)
            jsdoc = None
            continue
        # the not condition facilitates only getting preceding content
        elif not seen_jsdoc and "jsdoc_untagged" in groups:
            jsdoc = groups["jsdoc_untagged"]
            jsdocs.append(jsdoc)
            jsdoc = None
            continue
        elif "object_scope" in groups:
            object_scope = groups["object_scope"]
            debug_print(f"parse_ts: SET OBJECT_SCOPE = '{object_scope}'")

        if component:
            if jsdocs:
                jsdocs = ["/**"] + jsdocs + [" */\n"]
                jsdoc_stack = "\n".join(jsdocs)
                component = jsdoc_stack + component
                jsdoc_stack = ""
                jsdocs = []
                seen_jsdoc = False
            debug_print(f"{match_number=} FINAL component:", component)
            components.append(component)

    return components


# CPP_DENY = {"private", "public"}
# missed a lot
# def parse_cpp(contents: str) -> List[str]:
#     debug_print("parse_cpp")
#     contents = remove_c_comments(contents)

#     # Combined regex pattern to match all components
#     combined_pattern = re.compile(
#         # class or struct
#         r"\n((class|struct)? \w+( : \w+( \w+)?)?)|"
#         # enums (maybe class)
#         r"\n((enum) (class )?\w+( : \w+)?)|"
#         # (maybe template) functions
#         # r"\n((template<.*?>)?\n(\w+::)?(\w+) \w+\([\s\S]*?\))",
#         # r"((template<.*?>)?\n(\w+::)?(\w+)\s+\w+\([\s\S]*?\))",
#         # templates
#         # r"\n(template.*)\n|"
#         r"\n(template ?<.*?>[^\{^;^=]*)|"
#         # functions
#         # r"\n(((\w+::)?(\w+))\s+\w+\([\s\S]*?\))",
#         # r"\n(\b[\w:]+(?:<[^>]*>)?\s+\w+\s*\([^)]*\)\s*)\s*{",
#         r"\n(\b[\w:]+(?:<[^>]*>)?\s+\w+\s*\([^)]*\))\s*(?=\{)",
#         re.DOTALL,
#     )

#     components = []

#     for n, match in enumerate(combined_pattern.finditer(contents)):
#         debug_print(f"parse_cpp {n=} {match=}")
#         groups = extract_groups(match)
#         component = match.group().strip()
#         # fix a minor visual defect
#         component = component
#         if component in CPP_DENY:
#             continue
#         components.append(component)

#     return components


# BUG: catastrophic backtracking in some c files
# @func_set_timeout(1)
# def parse_c(contents: str) -> List[str]:
#     debug_print("parse_c")
#     contents = remove_c_comments(contents)
#     debug_print("comments removed")

#     # Combined regex pattern to match functions (including pointer return types), structs, enums, and typedefs
#     combined_pattern = re.compile(
#         # Functions (including pointer return types)
#         # r"\n((?:[\w*]+\s*)+\*?\s*\w+\s*\([^)]*\)\s*\{[^}]*\})|"
#         r"\n((?:[\w*]+\s*)+\*?\s*\w+\s*\([^)]*\)\s*)|"
#         # Structs
#         r"\n(static )?struct\s+\w+\s*\{[^}]*\}|"
#         # Enums
#         r"\nenum\s+\w+\s*\{[^}]*\}|"
#         # Typedefs
#         r"\ntypedef\s+struct\s*\{[^}]*\}\s*\w+;",
#         re.DOTALL,
#     )
#     debug_print("compiled pattern")

#     components = []

#     for n, match in enumerate(combined_pattern.finditer(contents)):
#         debug_print(f"parse_c {n=} {match=}")
#         _ = extract_groups(match)
#         component = match.group().strip()
#         if component.startswith("typedef"):
#             # Extract only the typedef struct name
#             typedef_name = component.split("}")[1].split(";")[0].strip()
#             component = f"typedef struct {typedef_name}"
#         else:
#             # Extract only the first line for each component
#             component = component.split("{")[0].strip()

#         components.append(component)

#     return components


# Define the regular expression pattern for comments
py_rb_comment_pattern = re.compile(r"\s*#.*\n")


def remove_py_comments(input_string: str) -> str:
    "Replace # comments with a newline character"
    return re.sub(py_rb_comment_pattern, "\n", input_string)


# Combined regex pattern to match Python components
combined_py_pattern = re.compile(
    # Functions and Methods, capturing indentation and multiline signatures
    r"^( *def\s+\w+(\[.*\])?\s*\((?:[^()]|\((?:[^()]|\([^()]*\))*\))*\)\s*(?:->\s*[\w\"'\[\],. ]+)?)\s*:|"
    # Classes
    r"(class \w+(\[.*\])?(\([\w\[\]\s,\.]*\))?):|"
    # Decorators
    r"^( *@\w+(\(.*\))?)\n|"
    # TypeVar
    r"\n(\w+ = TypeVar\([^)]+\))|"
    # Version
    r"\n((__version__ = \".*\"))",
    re.MULTILINE,
    # re.DOTALL,  # causes dramatic over-extension of matches
)


# r"( *def\s+\w+\s*\((?:[^()]|\((?:[^()]|\([^()]*\))*\))*\)\s*->\s*[\w\[\], ]+\s*):|"
# regex might help with indentation & multiline function signatures
# from pygments.lexers.python import PythonLexer


def parse_py(contents: str) -> List[str]:
    debug_print("parse_py")

    # remove comments in multiline signatures
    contents = remove_py_comments(contents)
    components = []
    decorator_carry = []
    for match_number, match in enumerate(combined_py_pattern.finditer(contents)):
        groups = extract_groups(match)
        debug_print(f"parse_py: {match_number=} {match=}")
        component = None
        # Function or Method is 1
        if 1 in groups:
            component = groups[1]
            if decorator_carry:
                component = "\n".join(decorator_carry) + "\n" + component
                decorator_carry = []

        # Class is 2
        elif 3 in groups:
            component = groups[3]
            if decorator_carry:
                component = "\n".join(decorator_carry) + "\n" + component
                decorator_carry = []
        # 6 is the whole decorator group
        elif 6 in groups:
            decorator = groups[6]
            decorator_carry.append(decorator)
        # 8 wraps entire TypeVar matches
        elif 8 in groups:
            component = groups[8]
        # 9 is __version__
        elif 9 in groups:
            component = groups[9]

        if component:
            assert isinstance(component, str)
            # component = Syntax(component, "python")
            components.append(component)

    return components


def parse_rb(contents: str) -> List[str]:
    debug_print("parse_rb")
    contents = remove_py_comments(contents)

    combined_pattern = re.compile(
        # Match method definitions (instance and class methods) with parameters
        r"^( *def\s+(self\.)?\w+[\w=]*(?:\s*\([^)]*\))?)|"
        # Match class and module definitions
        r"^( *class\s+\w+(?:\s*<\s*\w+)?|\bmodule\s+\w+)",
        re.MULTILINE,
    )

    components = []

    for n, match in enumerate(combined_pattern.finditer(contents)):
        debug_print(f"parse_rb: {n=} {match=}")
        component = None
        groups = extract_groups(match)
        component = groups[0]
        if component:
            debug_print(f"parse_rb {component=}")
            components.append(component)

    return components


def parse_fsharp(contents: str) -> List[str]:
    debug_print("parse_fsharp")

    combined_pattern = re.compile(
        # module or type in a single line
        r"^(\s*(module|type)\s+[^\n]+)|"
        # start of a let binding
        r"(\blet\b\s+(?:\w+\s+)*\w+\s*"
        # multiline parameters
        r"(?:\n\s+\(.+\)\s*)*"
        # return type and start of the body
        r":?\s*[^\n=]*=)",
        re.MULTILINE,
    )
    components = []
    for n, match in enumerate(combined_pattern.finditer(contents)):
        component = match.group().strip()
        debug_print(f"parse_fsharp match {n}:\n", component)
        components.append(component)

    return components


def parse_tcl(contents: str) -> List[str]:
    debug_print("parse_tcl")

    combined_pattern = re.compile(
        r"^(proc \w+ \{[^\}]*\})",
        re.MULTILINE,
    )
    components = []
    for n, match in enumerate(combined_pattern.finditer(contents)):
        debug_print(f"parse_tcl match {n}:\n", match)
        components.append(match.group())

    return components


def parse_erl(contents: str) -> List[str]:
    debug_print("parse_erl")

    combined_pattern = re.compile(
        # -spec 1
        r"^(-spec[^\n]*?(?:\n\s+.*?)*?\.)\n|"
        # -record(thingy). 2
        r"^((-record\(\w*))(?=,)|"
        # type 4
        r"^(-(?:type|opaque) \w+\((?:\s|[^\)])*\))(?=\s::)|"
        # -module(name). 5
        r"^((-module\([^\)]*\).))",
        re.MULTILINE,
    )

    components = []
    for n, match in enumerate(combined_pattern.finditer(contents)):
        groups = extract_groups(match)
        debug_print(f"parse_erl match {n}:\n", groups)
        component = None
        if 1 in groups:  # spec
            component = groups[1]
        elif 2 in groups:  # record
            component = groups[2] + ")."
        elif 4 in groups:  # type
            component = groups[4] + "."
        elif 5 in groups:  # module
            component = groups[5]

        if component:
            debug_print(f"parse_erl component {n}:\n{component}")
            components.append(component)

    return components


# combined_pattern = re.compile(
#     # functions
#     # r"(pub\s+)?(fn\s+[a-z_][a-z_0-9]*\s*(<[^>]+>)?\s*\([^)]*\)\s*(->\s*[^{]*)?(where\s*[^{]*)?|"
#     # r"\n( *(pub )?(async )?fn [\w_]*(\<.*\>)?(\([^)]*\))( -> ([^;{]*))?)|"
#     # r"\n?( *(pub )?(async )?fn [\w_]*(\<.*\>)?\(([^{]*?(?=\) -> ))(([^;{])*)?)|"
#     r"\n(?P<indent>\s*)(?P<pub>pub\s+)?(?P<async>async\s+)?fn\s+(?P<name>[\w_]+)(?P<generics><.*>)?\((?P<params>(?:[^()]+|\((?P<nested>[^()]+)\))*)\)(?P<return_type>\s+->\s+(?P<return>[^;{]*))?(?P<where_clause>\s*(?:where\n(?P<where>.*)?)?)?|"
#     # structs and impls with generics
#     # r"struct\s+[A-Z]\w*|impl\s+([A-Z]\w*)(\s+for\s+[A-Z]\w*)?|"
#     # r"^( *(pub)? ?struct\s+[A-Z]\w*|impl\s+([A-Z]\w*)(\s+for\s+[A-Z]\w*)?)"
#     r"\n(( *(((pub )?struct)|impl))[^{;]*) ?[{;]|"
#     # trait, enum, or mod
#     # r"trait\s+[A-Z]\w*|enum\s+[A-Z]\w*|mod\s+[a-z_][a-z_0-9]*|"
#     r"\n(( *)?(pub )?(trait|enum|mod)\s+\w*(<[^{]*>)?)|"
#     # r"macro_rules!\s+[a-z_][a-z_0-9]*)",
#     r"\n((#\[macro_export\]\n)?macro_rules!\s+[a-z_][a-z_0-9]*)",
#     re.MULTILINE,
# )
# Combined regex pattern to match all components
# combined_pattern = re.compile(
#     # functions
#     r"\n(?P<function_match>\s*(?P<function_pub>pub\s+)?(?P<function_async>async\s+)?fn\s+(?P<function_name>[\w_]+)(?P<function_generics><.*>)?\((?P<function_params>(?:[^()]+|\((?P<function_nested>[^()]+)\))*)\)(?P<function_return_type>\s+->\s+(?P<function_return>[^;{]*))?(?P<function_where_clause>\s*(?:where\n(?P<function_where>.*)?)?)?)|"
#     # structs and impls with generics
#     r"\n(?P<struct_impl_match>(( *((?P<struct_impl_pub>pub\s+)?struct)|impl))[^{;]*) ?[{;]|"
#     # trait, enum, or mod
#     r"\n(?P<trait_enum_mod_match> *(?P<trait_enum_mod_pub>pub\s+)?(trait|enum|mod)\s+\w*(<[^{]*>)?)|"
#     # macro
#     r"\n(?P<macro_match>((#\[macro_export\]\n)?macro_rules!\s+[a-z_][a-z_0-9]*))",
#     re.MULTILINE,
# )


def parse_rs(contents: str) -> List[str]:
    debug_print("parse_rs")

    contents = remove_c_comments(contents)

    combined_pattern = re.compile(
        # functions
        r"\n(?P<function>\s*(?:pub\s+)?(?:async\s+)?fn\s+(?:[\w_]+)(?:<.*>)?\((?P<function_params>(?:[^()]+|\((?P<function_nested>[^()]+)\))*)\)(?P<function_return_type>\s+->\s+(?:[^;{]*))?(?P<function_where_clause>\s*(?:where\n(?P<function_where>.*)?)?)?)|"
        # structs and impls with generics
        r"\n(?P<struct_impl>(?: *((?:pub\s+)?struct)|impl)[^{;]*?) ?[{;]|"
        # trait, enum, or mod
        r"\n(?P<trait_enum_mod> *(?:pub\s+)?(trait|enum|mod)\s+\w*(<[^{]*>)?)|"
        # macro
        r"\n(?P<macro>((#\[macro_export\]\n)?macro_rules!\s+[a-z_][a-z_0-9]*))",
        re.MULTILINE,
    )
    components = []

    for n, match in enumerate(combined_pattern.finditer(contents)):
        groups = extract_groups(match)
        component = None
        # functions
        if groups.get("function"):
            component = groups["function"].rstrip().rstrip(",")
        # struct or impl
        elif groups.get("struct_impl"):
            component = groups["struct_impl"].rstrip()
        # trait, enum, mod
        elif groups.get("trait_enum_mod"):
            component = groups["trait_enum_mod"]
        # macro
        elif groups.get("macro"):
            component = groups["macro"]
        if component:
            components.append(component.lstrip("\n"))

    return components


def parse_csv(contents: str, max_leaves=11) -> List[str]:
    debug_print("parse_csv")

    rows = contents.splitlines()

    columns = rows[0].split(",")
    n_cols = len(columns)
    if n_cols <= max_leaves:
        return columns

    return [f"{len(columns)} columns"]


def parse_mathematica(contents: str) -> List[str]:
    combined_pattern = re.compile(
        r"((\w+\[.*?\]))\s*:=.*?(?=\n\n|\Z)",
    )

    components = []

    for match_number, match in enumerate(combined_pattern.finditer(contents)):
        debug_print(f"parse_mathematica match_number: {match_number}")
        groups = extract_groups(match)
        component = groups[1]
        components.append(component)

    return components


def parse_r(contents: str) -> List[str]:
    combined_pattern = re.compile(
        # class and whatever's inside
        r"class\(.*\)|"
        # arrow or equals function
        r".* ((<\-)|=) (function)\(",
    )

    components = []

    for match_number, match in enumerate(combined_pattern.finditer(contents)):
        debug_print(f"parse_r match_number: {match_number}")
        groups = extract_groups(match)
        component = match.group().strip().rstrip("(")
        components.append(component)

    return components


def parse_zig(contents: str) -> List[str]:
    debug_print("parse_zig")
    contents = remove_c_comments(contents)
    combined_pattern = re.compile(
        r"\n( *(pub )?fn \w+\([^)]*\) [^{]*) |"
        r"const\s+\w+\s*=\s*struct|"
        r"test\s+\"[^\"]+\"",
        re.DOTALL,
    )

    components = []

    for match_number, match in enumerate(combined_pattern.finditer(contents)):
        debug_print(f"parse_zig match_number: {match_number}")
        groups = extract_groups(match)  # this debug prints the matches
        if 1 in groups:
            component = groups[1]
        else:
            component = match.group().rstrip()
        components.append(component)

    return components


def parse_hs(contents: str) -> List[str]:
    components = []

    # Combined regex pattern for Haskell components
    combined_pattern = re.compile(
        # Data type declarations
        r"^data\s+([^\s]+)|"
        # Function type signatures
        r"^([\w']+\s*::(?:.|\n)*?)(?=\n\w|\n\n|\Z)",
        re.MULTILINE,
    )

    for match_number, match in enumerate(combined_pattern.finditer(contents)):
        debug_print(f"parse_hs match_number: {match_number}")
        groups = extract_groups(match)
        if groups:
            component = groups[0].strip()
            debug_print(component)
            components.append(component)

    return components


def parse_lisp(content: str) -> List[str]:
    components = []

    # Combined regex pattern for various Lisp components
    combined_pattern = re.compile(
        # Common Lisp struct
        # r"\(defstruct\s+(\w+)|"
        # Common Lisp function
        # r"\(defun\s+(\w+)|"
        # Clojure protocol
        # r"\(defprotocol\s+(\w+)|"
        # Clojure record
        # r"\(defrecord\s+(\w+)|"
        # defn, means something other than def, clearly
        # r"\(defn\s+-?\s*(\w+)|"
        # just "def"
        # r"\(def\s+-?\s*(\w+)|"
        # Clojure namespace
        # Scheme define
        # r"\(define\s+\((\w+)|"
        # Macros
        # r"\(defmacro\s+(\w+)",
        # namespace
        r"\(ns\s+([^\s\(\)]+)|"
        # def(______)?
        r"( *\(def\w* \(?[\w_-]+)|"
        # racket has a struct
        r"\((struct \(?[\w_-]+)",
        re.DOTALL,
    )

    for match_number, match in enumerate(combined_pattern.finditer(content)):
        debug_print(f"parse_lisp match_number: {match_number}")
        groups = extract_groups(match)
        if groups:
            components.append(groups[0].replace("(", ""))

    return components


def parse_capnp(contents: str) -> List[str]:
    lines = contents.split("\n")
    components = []

    for line in lines:
        line_stripped = line.strip()
        leading_spaces = len(line) - len(line.lstrip())

        if (
            line_stripped.startswith("struct")
            or line_stripped.startswith("enum")
            or ":union" in line
        ):
            components.append(" " * leading_spaces + line_stripped.rstrip("{ "))
        elif "@" in line:
            field_info = " ".join(line_stripped.split()[:3]).rstrip(";")
            components.append(" " * leading_spaces + field_info)

    return components


def parse_grpc(contents: str) -> List[str]:
    lines = contents.split("\n")
    components = []

    for line in lines:
        line_stripped = line.strip()
        if line_stripped.startswith("syntax"):
            components.append(line_stripped.rstrip(";"))
        elif line_stripped.startswith("service") or line_stripped.startswith("message"):
            components.append(line_stripped.rstrip("{ "))
        elif line_stripped.startswith("rpc"):
            components.append("    " + line_stripped.rstrip(" {}"))
        elif (
            line_stripped
            and not line_stripped.startswith("//")
            and "=" in line_stripped
        ):
            field_info = line_stripped.split(";")[0]  # Retain field numbers
            components.append("    " + field_info)

    return components


def parse_openrpc_json(contents: str) -> List[str]:
    data = json.loads(contents)
    components = []

    components.append(f"openrpc: {data.get('openrpc', 'N/A')}")
    info = data.get("info", {})
    components.append("info:")
    components.append(f"    title: {info.get('title', 'N/A')}")
    components.append(f"    version: {info.get('version', 'N/A')}")

    methods = data.get("methods", [])
    components.append("methods:")
    for method in methods:
        method_name = method.get("name")
        method_desc = method.get("description", "No description")
        components.append(f"    {method_name}: {method_desc}")
        params = method.get("params", [])
        components.append("        params:")
        for param in params:
            param_type = param.get("schema", {}).get("type", "N/A")
            components.append(f"            - {param.get('name')}: {param_type}")
        result = method.get("result", {}).get("name", "N/A")
        result_desc = method.get("result", {}).get("description", "No description")
        components.append(f"        result: {result} = {result_desc}")

    return components


def parse_json_rpc(contents: str) -> List[str]:
    data = json.loads(contents)
    components = []

    components.append(f"jsonrpc: {data.get('jsonrpc', 'N/A')}")
    components.append(f"method: {data.get('method', 'N/A')}")
    components.append("params:")
    for param, value in data.get("params", {}).items():
        components.append(f"    {param}: {value}")
    components.append(f"id: {data.get('id', 'N/A')}")

    return components


def parse_graphql(contents: str) -> List[str]:
    components = []
    for line in contents.splitlines():
        line = line.strip()
        if line == "}":  # Skip lines that only contain a closing brace
            continue
        if line.startswith("type") or line.startswith("enum"):
            components.append(line.rstrip(" {"))  # Remove trailing '{'
        elif line and not line.startswith("#"):
            components.append("    " + line)
    return components


def format_dependency(name, details):
    if isinstance(details, str):
        return f"  {name} {details}"
    elif isinstance(details, dict):
        # Handle complex dependencies with additional properties
        version = details.get("version", "")
        features = ", ".join(details.get("features", []))
        return (
            f"  {name} {version} (features: {features})"
            if features
            else f"  {name} {version}"
        )
    return f"  {name}"


def parse_cargo_toml(contents: str) -> List[str]:
    debug_print("parse_cargo_toml")
    import tomli

    data = tomli.loads(contents)
    components = []

    if "package" in data:
        package_info = data["package"]
        components.append(f"name: {package_info.get('name', 'N/A')}")
        components.append(f"version: {package_info.get('version', 'N/A')}")
        components.append(f"description: {package_info.get('description', 'N/A')}")
        components.append(f"license: {package_info.get('license', 'N/A')}")

    if "dependencies" in data:
        dependencies = data["dependencies"]
        components.append("dependencies:")
        for dep, details in dependencies.items():
            components.append(format_dependency(dep, details))

    return components


def parse_pyproject_toml(contents: str) -> List[str]:
    debug_print("parse_pyproject_toml")
    import tomli

    data = tomli.loads(contents)
    debug_print("parse_pyproject_toml data:", data)
    components = []

    project_info = None
    if "project" in data:
        project_info = data["project"]
        debug_print("parse_pyproject_toml project_info:", project_info)
        components.append(f"name: {project_info.get('name', 'N/A')}")
        components.append(f"version: {project_info.get('version', 'N/A')}")
        project_description = project_info.get("description", "N/A")

        debug_print("parse_pyproject_toml project_description:", project_description)
        components.append(f"description: {project_description}")

        if "classifiers" in project_info:
            classifiers = project_info["classifiers"]
            for classifier in classifiers:
                if "License ::" in classifier:
                    components.append(classifier)

    if project_info and "dependencies" in project_info:
        dependencies = project_info["dependencies"]
        components.append("dependencies:")
        for dep in dependencies:
            components.append(f"    {dep}")

    return components


def parse_lean(lean_content: str) -> List[str]:
    debug_print("parse_lean")
    components = []

    # Regex for title, sections, lemmas, theorems, and axioms
    title_re = r"\/-!\n(# [^\n]+)"

    # Extract title
    title = re.search(title_re, lean_content)
    if title:
        title = title.group(1)
        debug_print("parse_lean: title", title)
        components.append(title)

    combined_pattern = re.compile(
        # organization
        r"(section|end) ([^\n]+)|"
        # content
        # r"(lemma|theorem|axiom) ([^\n]+?):\n",
        r"(lemma|theorem|axiom) ([^\n]+(?:\n\s+[^\n]+)*?)\s*:\n",
        re.DOTALL,
    )
    # Extract sections, lemmas, theorems, and axioms
    for i, match in enumerate(re.finditer(combined_pattern, lean_content)):
        component = match.group()
        debug_print(f"parse_lean component {i}:")
        debug_print(component)
        component = match.group(0).rstrip(" :\n")
        components.append(component)

    return components


def parse_cs(contents: str) -> List[str]:
    # Updated regex pattern to match all components including method signatures
    combined_cs_pattern = re.compile(
        # Interfaces, Enums, Delegates, Structs, Classes
        r"\n( *(public )?(static )?(interface|enum|delegate|struct|class)\s+(\w+)( \w+)?( : \w+)?)|"
        # Decorators in brackets like [HttpGet ...]
        r"\n\[.*\]|"
        # Methods in Interfaces and Classes, capturing indentation and signatures
        r"\n( *(public )?(static\s+)?[\w<>\[\],\s]+(?:\??)\s+(\w+)\s*\([^)]*\))(?! =>)|"
        # Namespaces
        r"\b(namespace\s+([\w\.]+))|"
        # Method Arrow functions, capturing signatures
        r"\n( *(?:public\s+)?(override\s+)?[\w<>\[\],\s]+\s+(\w+)\s*\([^)]*\)\s*=>)|"
        # Arrow functions, capturing signatures
        r"\n( *(var|Func<[\w<>,\s]+)\s+(\w+)\s*=\s*\([^)]*\)\s*=>)|"
        # Methods returning Lambda Expressions, capturing signatures
        r"\n( *(public\s+)?Func<[\w<>,\s]+\s+\w+)\([^)]*\)\s*\{|"
        # Event Handler Arrow Functions, capturing signatures
        r"\n( *\w+(\.\w+)*\s*\+=\s*(async )?\([^)]*\)\s*=>)",
        re.DOTALL,
    )

    components = []

    for match_number, match in enumerate(combined_cs_pattern.finditer(contents)):
        debug_print(f"parse_cs: {match_number=} {match=}")
        groups = extract_groups(match)
        # Interfaces, Enums, Delegates, Structs, Classes
        if 1 in groups:
            component = groups[1]
        # Methods in Interfaces and Classes
        elif 8 in groups:
            component = groups[8].lstrip("\n")
            if component.lstrip().startswith("private"):
                debug_print("parse_cs: skipping a private method")
                continue
            if no_return_type_found := re.search(r"^ +\w+\(", component):
                debug_print(f"parse_cs: {no_return_type_found=}")
                continue
        # Method Arrow functions Lambda 14
        elif 14 in groups:
            component = groups[14].lstrip("\n")
        # Arrow functions 17
        elif 17 in groups:
            component = groups[17]
        # Methods returning Lambda Expressions
        elif 20 in groups:
            component = groups[20]
        # Event Handler Arrow Functions
        elif 22 in groups:
            component = groups[22]
        # Namespaces
        else:
            component = match.group().strip()
        debug_print(f"parse_cs: {match_number=} {component=}")
        if component:
            components.append(component)

    return components


def parse_tex(tex_content: str) -> List[str]:
    debug_print("parse_tex")

    # Regex for title, author, and date
    title_re = r"\\title\{([^\}]+)\}"
    author_re = r"\\author\{((?:[^{}]+|\{[^\}]*\})*)\}"
    date_re = r"\\date\{([^\}]+)\}"

    # Extract title, author, and date
    title = re.search(title_re, tex_content)
    author = re.search(author_re, tex_content)
    date = re.search(date_re, tex_content)

    components = []
    if title:
        components.append(title.group(1))

    # Handle multiple authors
    if author:
        author = author.group(1)
        debug_print(f"parse_tex: {author=}")
        is_multi_author = author.count("\\and") > 0
        # Remove LaTeX commands
        author = re.sub(r"\\.*", "", author).strip()
        debug_print(f"parse_tex: Removed LaTeX commands {author=}")
        if is_multi_author:
            debug_print(f"parse_tex: multi {author=}")
            author = author.splitlines()[0]
            first_author = author.split("\\and")[0]
            author = re.sub(r"\\.*", "", first_author).strip() + " et al."
        components.append(author)

    if date:
        components.append(date.group(1))

    # Regex for sections and subsections
    section_re = r"\\(sub)?section\{([^\}]+)\}"

    # Extract sections and subsections
    outline = []
    section_count = 0
    subsection_count = 0
    for match in re.finditer(section_re, tex_content):
        if match.group(1):  # Subsection
            subsection_count += 1
            outline.append(f"  {section_count}.{subsection_count} {match.group(2)}")
        else:
            section_count += 1
            subsection_count = 0  # Reset
            outline.append(f"{section_count} {match.group(2)}")
    debug_print("parse_tex outline:", outline)
    components.extend(outline)
    return components


def parse_go(contents: str) -> List[str]:
    debug_print("parse_go")

    # Combined regex pattern to match Go components
    combined_pattern = re.compile(
        # struct or type declarations without the body
        r"^( *type \w+ (struct|interface))(?=\s*\{)|"
        # function declarations, including multiline, without the body
        # r"\n(func[\s?\S?]*?){\n",
        # r"\s(func [\s\S]+?){\s",
        r"^((func [\s\S]+?))(?=\s{\s)",
        re.MULTILINE,
    )

    components = []

    for n, match in enumerate(combined_pattern.finditer(contents)):
        debug_print(f"parse_go: {n=} {match=}")
        component = None
        groups = extract_groups(match)
        if 1 in groups:
            component = groups[1]
        elif 3 in groups:
            component = groups[3]
        if component:
            debug_print(f"parse_go final {component=}")
            components.append(component)

    return components


def parse_swift(contents: str) -> List[str]:
    debug_print("parse_swift")

    contents = remove_c_comments(contents)

    # Combined regex pattern to match Go components
    combined_pattern = re.compile(
        # # class, enum, enum class, with or without protocol
        # r"\n((enum)?( ?class|struct)? \w+:? ?\w+)|"
        # # protocols are that which is inherited
        # r"\n((protocol) \w+)|"
        r"^((?:class|struct|protocol|enum).*(?= {))|"
        # functions or init methods, with or without multiline signatures
        # r"\n( *func [\s\S]+?)(?=\s{\s)",
        r"^( *(?:func|init)\s*\w*\s*\([^)]*\)\s*(?:->\s*\w+\s*)?(?=(?: \{|;|\n)))",
        re.MULTILINE,
    )

    components = []

    for n, match in enumerate(combined_pattern.finditer(contents)):
        debug_print(f"parse_swift {n=} {match=}")
        groups = extract_groups(match)
        component = None
        if 1 in groups:
            component = groups[1]
        else:
            component = groups[0]
        # elif
        # component = match.group()
        if component:
            debug_print(f"parse_swift {component=}")
            components.append(component)

    return components


def parse_bash(contents: str) -> List[str]:
    debug_print("parse_bash")

    # Combined regex pattern to match Go components
    combined_pattern = re.compile(
        # aliases
        r"\n(alias .*)|"
        # functions
        r"\n?((function)?\s+\w+\(\))\n?|"
        # exports without values
        r"\n(export \w+)=",
        # re.DOTALL, # this causes weird issues with the alias matching
    )

    components = []

    for match in combined_pattern.finditer(contents):
        debug_print(f"{match=}")
        debug_print(f"{match.groups()=}")
        component = match.group().strip().replace(" {", "")
        debug_print(f"{component=}")
        if component.startswith("export"):
            # remove equals signs
            component = component[:-1]
        elif "\n" in component:
            component_lines = component.splitlines()
            component = component_lines[0].strip()
        debug_print(f"final {component=}")
        components.append(component)

    return components


# (declare|export) (default)?(\w+ \w+(<.*>)?(\(.*\))?(: \w+)?)
def parse_d_dot_ts(contents: str) -> List[str]:
    debug_print(contents)
    pattern = r"(declare|export) (default)?(\w+ \w+(<.*>)?(\(.*\))?(: \w+)?)"
    keepers = []
    if matches := re.findall(pattern, contents):
        for matched in matches:
            debug_print(matched)
    return keepers


def parse_angular_app_module(contents: str) -> List[str]:
    debug_print("parse_angular_app_module")
    pattern = r"(@NgModule\({\n\s*declarations: \[((\n\s*)[a-zA-Z]*,?)*)\n"
    if matches := re.search(pattern, contents, re.DOTALL):
        debug_print(f"{matches=}")
        return [matches[1]]
    return []


def parse_angular_routes(content: str) -> List[str]:
    routes_pattern = r"(const routes: Routes = \[\n(?:.|\n)*?\];)"
    routes_match = re.findall(routes_pattern, content)
    if routes_match:
        debug_print(f"{routes_match=}")
        return [routes_match[0]]
    return []


def parse_angular_spec(content: str) -> List[str]:
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


def parse_environment_ts(contents: str) -> List[str]:
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


def parse_dot_env(contents: str) -> List[str]:
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


def parse_requirements_txt(contents: str) -> List[str]:
    debug_print(f"parse_requirements_txt")
    return [line for line in contents.splitlines() if not line.startswith("#")]


def parse_json_schema(contents: str) -> List[str]:
    debug_print("parse_json_schema")
    parsed = json.loads(contents)
    keepers = []
    debug_print(parsed)
    for key in ["$schema", "type", "title", "description"]:
        if key in parsed:
            keepers.append(f"{key}: {parsed[key]}")
    return keepers


def parse_package_json(contents: str) -> List[str]:
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


def is_openapi_yml(ymls: Tuple[dict]) -> bool:
    yml = ymls[0]
    return "openapi" in yml or "swagger" in yml


def is_k8s_yml(ymls: Tuple[dict]) -> bool:
    yml = ymls[0]
    if "apiVersion" in yml and "kind" in yml and "metadata" in yml:
        return True
    return False


def is_ansible_yml(ymls: Tuple[dict]) -> bool:
    yml = ymls[0]
    if isinstance(yml, list) and any(
        isinstance(item, dict) and "name" in item for item in yml
    ):
        return True
    return False


def is_github_yml(ymls: Tuple[dict]) -> bool:
    yml = ymls[0]
    if "name" in yml and "jobs" in yml:
        return True
    return False


def parse_github_yml(ymls: Tuple[dict]) -> List[str]:
    result = []
    yml = ymls[0]
    result.append(yml["name"])
    for job in yml["jobs"]:
        result.append(f"  job: {job}")
        if "steps" in yml["jobs"][job]:
            for step in yml["jobs"][job]["steps"]:
                if "name" in step:
                    result.append(f"    - {step['name']}")
    return result


def parse_k8s(ymls: Tuple[dict]) -> List[str]:
    result = []
    for yml in ymls:
        result.append(f"{yml['apiVersion']}.{yml['kind']} -> {yml['metadata']['name']}")
    return result


def parse_ansible(ymls: Tuple[dict]) -> List[str]:
    result = []
    for yml in ymls:
        result.extend(item.get("name", "") for item in yml if isinstance(item, dict))
    return result


def parse_openapi_yml(ymls: Tuple[dict]) -> List[str]:
    components = []
    yml = ymls[0]
    # Extract OpenAPI version and info
    components.append(f"openapi: {yml.get('openapi', yml.get('swagger', 'N/A'))}")
    info = yml.get("info", {})
    components.append(f"    title: {info.get('title', 'N/A')}")
    description = info.get("description", "")
    first_sentence = (
        re.split(r"\.\s|\.\n", description, maxsplit=1)[0] + "."
        if description
        else "N/A"
    )
    components.append(f"    description: {first_sentence}")
    components.append(f"    version: {info.get('version', 'N/A')}")

    # Extracting servers
    servers = yml.get("servers", [])
    if servers:
        components.append("servers:")
        for server in servers:
            components.append(f"    - url: {server.get('url', 'N/A')}")

    # Extracting paths
    paths = yml.get("paths", {})
    if paths:
        components.append("paths:")
        for path, methods in paths.items():
            components.append(f"    '{path}':")
            for method, details in methods.items():
                operation_id = details.get("operationId", "N/A")
                summary = details.get("summary", "N/A")
                components.append(
                    f"        {method.upper()} ({operation_id}): {summary}"
                )

    return components


def parse_yml(contents: str) -> List[str]:
    import yaml

    try:
        ymls = tuple(yaml.safe_load_all(contents))
    except Exception as e:
        debug_print("parse_yml yaml.safe_load Exception:", e)
        debug_print("parse_yml contents:", contents)
        raise
    if not ymls:
        return []
    try:
        if is_k8s_yml(ymls):
            return parse_k8s(ymls)
        elif is_ansible_yml(ymls):
            return parse_ansible(ymls)
        elif is_github_yml(ymls):
            return parse_github_yml(ymls)
        elif is_openapi_yml(ymls):
            return parse_openapi_yml(ymls)
    except Exception as e:
        debug_print("parse_yml transform Exception: ", e)
    return ["Unsupported YAML Category"]


def parse_db(db_path: str) -> List[str]:
    # Connect to the SQLite database
    import sqlite3

    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()

    # Get the list of table names
    cursor.execute("SELECT name FROM sqlite_master WHERE type='table';")
    tables = cursor.fetchall()

    components = []

    # For each table, get the column details
    for table in tables:
        table_name = table[0]
        components.append(f"{table_name} table:")

        cursor.execute(f"PRAGMA table_info({table_name});")
        columns = cursor.fetchall()
        debug_print("parse_db columns:", columns)

        for column in columns:
            column_name = column[1]
            column_type = column[2].lower()
            is_primary_key = column[5] == 1
            not_null = column[3] == 1
            column_desc = f"   {column_name} {column_type}"
            if is_primary_key:
                column_desc += " primary key"
            if not_null and not is_primary_key:
                column_desc += " not null"
            components.append(column_desc)
    # Close the connection
    conn.close()

    return components


def dedent_components(components: List[str]) -> List[str]:
    dedent_amount = min(
        len(component) - len(component.lstrip(" ")) for component in components
    )
    if dedent_amount == 0:
        return components
    pattern = r"^(?P<indentation_to_remove> {," + str(dedent_amount) + r"})"
    debug_print("dedent_components PATTERN:", pattern)
    dedented_components = [re.sub(pattern, "", component) for component in components]
    return dedented_components


def parse_cbl(content: str) -> List[str]:
    debug_print("parse_cbl")

    # Regex pattern to match significant lines or sections with indentation
    combined_pattern = re.compile(
        r"^(?P<division> *\w+ division\.?)|"
        r"^(?P<program_id> *(program-id.)\s*[\w-]+(?P<program_id_tail>.|(?=\s))?)|"
        r"^(?P<author> *author. .*\.?(?=\s))|"
        r"^(?P<section> *[\w-]+ section.(?=\s))|"
        r"^(?P<file_control> *file-control.?(?=\s))|"
        r"^(?P<selection> *select [\w-]+(?=\s))|"
        r"^(?P<file_descriptor> *fd +[\w-]*)|"
        r"^(?P<numbered> *\d{2} *[\w-]*\.?)|"
        r"^(?P<date_line> *date.*\.?$)|"
        r"^(?P<nonnumbered_procedure> *[\w-]+\.)|"
        r"^(?P<end_program> *end program [\w-]+\.?)",
        re.MULTILINE | re.IGNORECASE,
    )

    components = []
    collecting_procedures = False
    candidate_procedures = []
    end_program = None
    for match_number, match in enumerate(combined_pattern.finditer(content)):
        debug_print(f"parse_cbl: match_number={match_number} match={match}")
        groups = extract_groups(match, named_only=True)
        component = None
        if "numbered" in groups:
            component = groups["numbered"]
        elif collecting_procedures and "nonnumbered_procedure" in groups:
            component = groups["nonnumbered_procedure"]
        elif "selection" in groups:
            component = groups["selection"]
        elif "section" in groups:
            component = groups["section"]
        elif "division" in groups:
            component = groups["division"]
            if "procedure" in component.lower():
                collecting_procedures = True
        elif "file_descriptor" in groups:
            component = groups["file_descriptor"]
        elif "file_control" in groups:
            component = groups["file_control"]
        elif "author" in groups:
            component = groups["author"]
        elif "date_line" in groups:
            component = groups["date_line"]
        elif "program_id" in groups:
            component = groups["program_id"]
        elif "end_program" in groups:
            end_program = groups["end_program"]

        if component:
            if not component.rstrip(" ").endswith("."):
                component = component.rstrip(" ") + "."
            # save procedures for later
            if collecting_procedures:
                if "nonnumbered_procedure" in groups or "numbered" in groups:
                    candidate_procedures.append(component)
                    continue
            components.append(component)

    # only the most dedented procedures
    procedures = []
    if candidate_procedures:
        dedented_candidate_procedures = dedent_components(candidate_procedures)
        debug_print("dedented_candidate_procedures", dedented_candidate_procedures)
        for i, dedented_candidate_procedure in enumerate(dedented_candidate_procedures):
            # skip indented candidates
            if dedented_candidate_procedure.startswith(" "):
                continue
            procedure = candidate_procedures[i]
            procedures.append(procedure)
    if procedures:
        components.extend(procedures)
    if end_program:
        components.append(end_program)
    # now we dedent again as a complete group
    if components:
        dedented_components = dedent_components(components)
        return dedented_components
    return components


def parse_java(contents: str) -> List[str]:
    debug_print("parse_java")
    contents = remove_c_comments(contents)
    # Combined regex pattern to match Java components
    combined_pattern = re.compile(
        # Classes
        r"\n( *(public )?(abstract )?class\s+(\w+)( extends \w+)?( implements [\w, ]+)?)\s*\{|"
        # Methods, capturing indentation
        # r"\n(( *(public|protected|private)?\s+(static\s+)?(abstract\s+)?[\w<>\[\],]+\??\s+(\w+))\([^)]*\))\s*(\{|;)|"
        # r"(( *@[\w\"/()]+\n)*\n *(public|protected|private)? ?(abstract|static)? ?(\w+ )?\w+\([^{]*\))( {\n)|"
        r"\n( *(public|protected|private)? ?(abstract|static)? ?(\w+ )?\w+\([^{]*\))( {\n)|"
        # Interfaces
        r"\n( *(public )?interface\s+(\w+))|"
        # Annotations
        # r"\n( *@\w+(\([^)]*\))?\s*)"
        r"( *@[\w\"/()]+)|"
        # Abstract and Interface methods
        r"\n( *(abstract)? \w+ \w+\([^)]*\));",
        re.DOTALL,
    )

    components = []
    for match_number, match in enumerate(combined_pattern.finditer(contents)):
        debug_print(f"parse_java: {match_number=} {match=}")
        groups = extract_groups(match)
        if 1 in groups:  # Class
            component = groups[1].rstrip()
        elif 7 in groups:  # Method
            component = groups[7].lstrip("\n")
        elif 11 in groups:  # Interface
            component = "interface " + groups[11]
        elif 15 in groups:  # Annotation
            component = groups[15]
        elif 16 in groups:  # Abstract Method
            component = groups[16]
        else:
            component = match.group().strip()

        debug_print(f"parse_java component:\n{component}")
        if component:
            components.append(component)

    return components


def parse_jl(contents: str) -> List[str]:
    debug_print("parse_jl")
    contents = remove_py_comments(contents)
    combined_pattern = re.compile(
        # traditional functions
        r"^ *(?P<signature>function \w+\([\s\S]*?\)(?: where ({.*}|.*))?)(?P<details>[\s\S]*?(?P<ending>\s^ *end))(?=\s)|"
        # assignment form of functions
        r"^ *\S+\((?:[\s\S]*?\)(?= =))|"
        # structs
        r"^( *(?:Base\.@kwdef )?(?:mutable )?struct \w+(?:[\s\S])*?(\s^\s*end))|"
        r"^(module \w+)|"
        r"^( *end)",
        re.MULTILINE,
    )

    components = []

    for n, match in enumerate(combined_pattern.finditer(contents)):
        debug_print(f"parse_jl: {n=} {match=}")
        groups = extract_groups(match)
        if 4 in groups:
            component = groups[1] + groups[4]
        else:
            component = groups[0]

        if component:
            components.append(component)

    return components


# edge case ends like \)( = \w+\(.*\))
def parse_kt(contents: str) -> List[str]:
    debug_print("parse_kt")
    # Combined regex pattern to match Kotlin constructs as single groups
    combined_pattern = re.compile(
        # 1. classes, interfaces, objects (no fun) # blank line group discovered here! learn more!!!
        r"(?m)(?:^ *(\w+ (?<!fun ))*(?:class|interface|object)[^.](?:[^{](?!(?:^\s*$)|^\w+))*)|"
        # 2. fun
        # r"(?m)( +[A-Z]+ {.*\n)?^.* ?fun (?:[^{}](?!(?:^\s*$)))*(?=(?:\s=\s)|(?: {)|(?:\s^}(?<!=)))",
        # r"( +[A-Z]+ {.*\n)?(^.* ?fun )(([^{}](?!(^\s*$)|=))*())(?=(\s^}(?<!\) = \w))|(\s=\s)|( {))",
        # r"( +[A-Z]+ {.*\n)?(^.* ?fun )(([^{}](?!(^\s*$)|($\s}))|( = ))*)(?=(\s^}(?<!\) = \w))|(\s=\s)|( {))",
        # r"( +[A-Z]+ {.*\n)?(^.* ?fun )(([^{}](?!(^\s*$))|( = ))*)(?=(\)\n^}(?<!\) = \w))|(\s=\s)|( {))",
        # r"(?m)( +[A-Z]+ {.*\n)?(^.* ?fun )(([^{}](?!(^\s*$))|( = ))*)(?=(\s^}(?<!\) = \w))|(\s=\s)|( {))",
        r"(?:(?: +[A-Z]+ {.*\n)?(?:^.* ?fun (?:<.*> )?(?:(?:\w|\.)\??|<.*>)*)\([^)]*\)(?:\s?: [A-Z]\w*\??)?(?:\s->\s\w+\))?)(?=\s)",
        re.MULTILINE,
    )

    components = []

    for match_number, match in enumerate(combined_pattern.finditer(contents)):
        debug_print(f"parse_kt: {match_number=} {match=}")
        groups = extract_groups(match)
        component = groups[0]
        if component:
            component = remove_c_comments(component).rstrip().replace(", \n", ",\n")
            components.append(component)

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


# TODO: update parse_objective_c to avoid fixed unrolling
def parse_objective_c(content: str) -> List[str]:
    components = []

    interface_pattern = r"@interface\s+(\w+).*?:\s+NSObject\s*"
    interface_matches = re.findall(interface_pattern, content, re.DOTALL)
    class_name = None

    for interface_match in interface_matches:
        class_name = interface_match
        components.append(f"@interface {class_name}")

    if class_name is not None:
        interface_method_pattern = (
            r"@interface\s+\w+.*?:\s+NSObject\s*[-\s]*\((.*?)\)\s+(.*?);\s*"
        )
        interface_method_matches = re.findall(
            interface_method_pattern, content, re.DOTALL
        )

        for interface_method_match in interface_method_matches:
            return_type = interface_method_match[0]
            method = interface_method_match[1].rstrip()
            components.append(f"@interface {class_name} -> ({return_type}) {method}")

        implementation_pattern = r"@implementation\s+(\w+)\s*\n"
        implementation_matches = re.findall(implementation_pattern, content, re.DOTALL)

        for implementation_match in implementation_matches:
            class_name = implementation_match
            components.append(f"@implementation {class_name}")

        implementation_method_pattern = (
            r"@implementation\s+\w+\s*\n- \((.*?)\)\s+(.*?){"
        )
        implementation_method_matches = re.findall(
            implementation_method_pattern, content, re.DOTALL
        )

        for implementation_method_match in implementation_method_matches:
            return_type = implementation_method_match[0]
            method = implementation_method_match[1].rstrip()
            components.append(
                f"@implementation {class_name} -> ({return_type}) {method}"
            )

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
        _, class_name, class_body = match.groups()
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


def parse_ps1(contents: str) -> List[str]:
    debug_print("parse_ps1")
    contents = remove_py_comments(contents)
    pattern = re.compile(
        # function
        r"^(?P<function>[ \t]*(?P<function_scope>\w+:)?(?:function|filter)[\s\S]*?(?=\s{))|"
        # r"^(?P<param_block> *(?:P|p)aram \((?:\s*(?:\[|\$|(?:\)(?=\n))).*)*)|"
        r"^(?P<param_oneliner>[ \t]+(?:P|p)aram ?\(.*?\).*?$)|"
        r"^(?P<param_block>\s*(?:P|p)aram ?\((?P<args>[\s\S]+?(?=^\s+\))))|"
        # method
        r"^(?P<method> *(?:\[.*\])?\w+\([\s\S]*?\)(?=\s{))|"
        # (P|p)aram
        # class
        r"^(?P<class>(?P<class_scope>\w+:)?class[\s\S]*?(?= {))|"
        # [Annotation] not followed by $
        r"^(?P<annotation> *\[.*\](?!\$))",
        re.MULTILINE,
    )
    components = []
    for n, match in enumerate(pattern.finditer(contents)):
        debug_print(f"parse_ps1 {n=} {match=}")
        groups = extract_groups(match, named_only=True)
        component = None
        if "function" in groups:
            component = groups["function"]
        elif "method" in groups:
            component = groups["method"]
        elif "class" in groups:
            component = groups["class"]
        elif "param_oneliner" in groups:
            component = groups["param_oneliner"]
        elif "param_block" in groups:
            component = groups["param_block"]
            component_lines = component.splitlines()
            keepers = []
            for m, line in enumerate(component_lines):
                debug_print(f"line {m}", line)
                lstripped = line.lstrip().lstrip("\t")
                if lstripped.startswith(")"):
                    break
                if not lstripped:
                    continue
                keepers.append(line)
            keepers[-1] = keepers[-1].rstrip("\n").rstrip("\t").rstrip()
            component = "\n".join(keepers)
            component = component.rstrip("\n").rstrip("\t") + ")"
        elif "annotation" in groups:
            component = groups["annotation"]
        debug_print(f"parse_ps1 {n=} {component=}")
        if component:
            # component = Text(component)
            components.append(component)
    return components


def parse_matlab(content: str) -> List[str]:
    components = []

    class_pattern = r"(classdef\s+\w+.*?end)"
    class_matches = re.findall(class_pattern, content, re.DOTALL)
    remaining_content = content

    for class_match in class_matches:
        class_content = class_match
        remaining_content = remaining_content.replace(class_content, "")

        class_name_pattern = r"classdef\s+(\w+)"
        class_name = re.search(class_name_pattern, class_content, re.DOTALL)
        if class_name:
            class_name = class_name.group(1)

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
    debug_print("parse_scala:")
    # Combined regex pattern to match Scala components
    combined_pattern = re.compile(
        # Function signatures
        r"^( *def[^{]*(?= =))|"
        # Trait, case class, object names
        r"^( *(?:trait|(case )?class|object)\s+\w+(?:\[([^\]])+\])?((?:\([^\)]*)\))?)",
        re.MULTILINE,
    )

    components = []

    for n, match in enumerate(combined_pattern.finditer(contents)):
        debug_print(f"parse_scala: {n=} {match=}")
        groups = extract_groups(match)
        component = groups[0]  # Get the matched component
        if component:
            components.append(component)

    return components


def parse_tf(contents: str) -> List[str]:
    pattern = r'(provider|resource|data|variable|output|locals|module)\s+("[^"]*"\s*"[^"]*"|"[^"]*"|\'[^\']*\'|[^\s]*)\s*[{"{]'
    matches = re.findall(pattern, contents, re.MULTILINE)
    return [f"{match[0]} {match[1]}" if match[1] else match[0] for match in matches]


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
                if task_match:
                    task_text = task_match.group(2)
                    is_checked = "[x]" in line or "[X]" in line
                    # print(f"Checked: {is_checked} Line: {line}")
                    task: str = (
                        indent_level * " "
                        + ("- [x] " if is_checked else "- [ ] ")
                        + task_text
                    )
                    # For every task, we first remove ancestors that are not parents of the current task
                    # This is identified by comparing their indentation level.
                    checked_ancestors: Sequence[Tuple[str, bool]] = [
                        a for a in checked_ancestors if len(a[0]) < len(task)
                    ]
                    # If the task is checked, we add it to the list of ancestors but don't add to the final output yet.
                    # The second element of the tuple is a flag to indicate whether this ancestor should be included in the output.
                    if is_checked:
                        ancestor_tuple: Tuple[str, bool] = (task, False)
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


marker_pattern = re.compile(r'(BUG|TODO|NOTE)(?![\'"]): (.*)')


def parse_markers(content: str) -> List[str]:
    markers = []
    lines = content.splitlines()
    for line_n, line in enumerate(lines, start=1):
        match = marker_pattern.search(line)
        if match:
            marker, content = match.groups()
            mark = f"{marker.upper()} (Line {line_n}): {content.strip()}"
            markers.append(mark)
    return markers
