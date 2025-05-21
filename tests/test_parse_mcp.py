import pytest
import asyncio
from typing import Dict, Any, List

# Assuming mcp_client is structured to allow importing parse_mcp_capabilities directly for testing
# or that parse_mcp_sync can be tested with a mocked fetch.
# For this initial test, we'll test parse_mcp_capabilities with mock data.
from tree_plus_src.mcp_client import parse_mcp_capabilities, format_tool_signature, format_arg

# Mock data based on the demo_server.py structure
MOCK_CAPABILITIES_DATA: Dict[str, Any] = {
    "name": "Tree+ Test",
    "description": "A test server for tree_plus using FastMCP.", # Assuming FastMCP adds its own desc
    "tools": [
        {
            "name": "add",
            "description": "Adds two integers.", # Assuming from docstring or FastMCP default
            "args_schema": {
                "type": "object",
                "properties": {
                    "a": {"type": "integer", "description": "The first integer."},
                    "b": {"type": "integer", "description": "The second integer."}
                },
                "required": ["a", "b"]
            },
            "output_type": {"type": "integer"}
        },
        {
            "name": "echo",
            "description": "Echoes the input text.",
            "args_schema": {
                "type": "object",
                "properties": {
                    "txt": {"type": "string", "description": "The text to echo."}
                },
                "required": ["txt"]
            },
            "output_type": {"type": "string"}
        }
    ],
    "resources": [
        {
            "path": "config://app",
            "description": "Returns the application demo config.", # Assuming from docstring
            "output_type": {"type": "string"} # Example, resource output types might not be in spec
        },
        {
            "path": "greeting://{name}",
            "description": "Greets the given name.",
            "output_type": {"type": "string"}
        }
    ],
    "prompts": [
        {
            "name": "review",
            "description": "Reviews the given code.",
            "args_schema": {
                "type": "object",
                "properties": {
                    "code": {"type": "string", "description": "The code to review."}
                },
                "required": ["code"]
            },
            # Assuming prompts also have an output_type, like tools
            "output_type": {"type": "string"} 
        }
    ],
    "models": [ # Example if server exposes models
        {
            "id": "example-model-001",
            "description": "An example language model."
        }
    ]
}

MOCK_SERVER_URL = "http://localhost:5123/mcp/"

# Test for format_arg (helper function, good to test individually)
@pytest.mark.parametrize("arg_name, arg_info, expected_output", [
    ("location", {"type": "string", "description": "The city"}, "location: string - 'The city'"),
    ("limit", {"type": "integer", "default": 10}, "limit: integer = 10"),
    ("verbose", {"type": "boolean", "default": False, "description": "Enable verbosity"}, "verbose: boolean = False - 'Enable verbosity'"),
    ("items", {"type": "array", "title": "List[str]"}, "items: List[str]"), # Pydantic v2 title
    ("optional_field", {"anyOf": [{"type": "string"}, {"type": "null"}], "description": "Can be null"}, "optional_field: string - 'Can be null'"),
    ("optional_int", {"anyOf": [{"type": "integer"}, {"type": "null"}], "default": None}, "optional_int: integer = None"),
])
def test_format_arg(arg_name, arg_info, expected_output):
    assert format_arg(arg_name, arg_info) == expected_output

# Test for format_tool_signature
@pytest.mark.parametrize("tool_name, tool_info, expected_output", [
    ("get_user", 
     {"args_schema": {"type": "object", "properties": {"user_id": {"type": "integer"}}, "required": ["user_id"]}, "output_type": {"type": "object", "title": "User"}},
     "get_user(user_id: integer) -> User"),
    ("list_items", 
     {"args_schema": {"type": "object", "properties": {"count": {"type": "integer", "default": 10}, "type": {"type": "string", "description": "Type of item"}}, "required": ["type"]}, "output_type": {"type": "array", "title": "List[Item]"}},
     "list_items(type: string - 'Type of item', count: integer = 10) -> List[Item]"), # Required first, then optional
    ("no_args_tool",
     {"args_schema": {}, "output_type": {"type": "string"}},
     "no_args_tool() -> string"),
    ("complex_args_tool", {
        "args_schema": {
            "type": "object",
            "properties": {
                "name": {"type": "string", "description": "Name of the entity"},
                "age": {"type": "integer", "default": 30, "description": "Age of the entity"},
                "category": {"type": "string", "enum": ["A", "B"], "default": "A"}
            },
            "required": ["name"]
        }, "output_type": {"type": "boolean"}
     },
     "complex_args_tool(name: string - 'Name of the entity', age: integer = 30 - 'Age of the entity', category: string = 'A') -> boolean")
])
def test_format_tool_signature(tool_name, tool_info, expected_output):
    assert format_tool_signature(tool_name, tool_info) == expected_output


@pytest.mark.asyncio
async def test_parse_mcp_capabilities_with_mock_data():
    # Act
    parsed_output: List[str] = await parse_mcp_capabilities(MOCK_CAPABILITIES_DATA, MOCK_SERVER_URL)

    # Assert
    assert parsed_output is not None
    assert len(parsed_output) > 0 # Check that some output is generated

    # Check server name and description
    assert f"MCP Server: {MOCK_CAPABILITIES_DATA['name']} ({MOCK_SERVER_URL})" in parsed_output
    assert f"  Description: {MOCK_CAPABILITIES_DATA['description']}" in parsed_output
    
    # Check for headings
    assert "Tools:" in parsed_output
    assert "Resources:" in parsed_output
    assert "Prompts:" in parsed_output
    assert "Models:" in parsed_output

    # Check tool formatting (example based on MOCK_CAPABILITIES_DATA)
    # Expected signature for 'add' tool based on new formatters:
    # add(a: integer - 'The first integer.', b: integer - 'The second integer.') -> integer
    expected_add_tool_signature = "add(a: integer - 'The first integer.', b: integer - 'The second integer.') -> integer"
    assert f"  Tool: {expected_add_tool_signature}" in parsed_output
    assert f"    Description: {MOCK_CAPABILITIES_DATA['tools'][0]['description']}" in parsed_output

    expected_echo_tool_signature = "echo(txt: string - 'The text to echo.') -> string"
    assert f"  Tool: {expected_echo_tool_signature}" in parsed_output
    assert f"    Description: {MOCK_CAPABILITIES_DATA['tools'][1]['description']}" in parsed_output

    # Check resource formatting
    assert f"  Resource: {MOCK_CAPABILITIES_DATA['resources'][0]['path']}" in parsed_output
    assert f"    Description: {MOCK_CAPABILITIES_DATA['resources'][0]['description']}" in parsed_output
    assert f"  Resource: {MOCK_CAPABILITIES_DATA['resources'][1]['path']}" in parsed_output
    assert f"    Description: {MOCK_CAPABILITIES_DATA['resources'][1]['description']}" in parsed_output
    
    # Check prompt formatting
    expected_review_prompt_signature = "review(code: string - 'The code to review.') -> string"
    assert f"  Prompt: {expected_review_prompt_signature}" in parsed_output
    assert f"    Description: {MOCK_CAPABILITIES_DATA['prompts'][0]['description']}" in parsed_output

    # Check model formatting
    model1 = MOCK_CAPABILITIES_DATA['models'][0]
    assert f"  Model: {model1['id']} - {model1['description']}" in parsed_output
    
    # Verify sorting (tools, resources, prompts, models should be sorted by name/path/id)
    # This is implicitly tested if the assertions for specific items pass in the expected order
    # or by checking indices if necessary. For now, specific item checks cover this.

    # Example of a more detailed check on ordering if needed:
    tools_section_index = parsed_output.index("Tools:")
    add_tool_line_index = -1
    echo_tool_line_index = -1
    for i, line in enumerate(parsed_output[tools_section_index:]):
        if "Tool: add(" in line:
            add_tool_line_index = tools_section_index + i
        elif "Tool: echo(" in line:
            echo_tool_line_index = tools_section_index + i
    
    assert add_tool_line_index != -1 and echo_tool_line_index != -1
    assert add_tool_line_index < echo_tool_line_index # 'add' comes before 'echo' alphabetically

    # A simple way to visually inspect during development/debugging:
    # for line in parsed_output:
    #     print(line)

# Placeholder for future test using the live server (requires server to be running)
# @pytest.mark.asyncio
# async def test_parse_mcp_with_live_server():
#     # This test would require the demo_server.py to be running
#     # from tree_plus_src.mcp_client import parse_mcp_sync
#     # live_url = "http://localhost:5123/mcp/"
#     # output = parse_mcp_sync(live_url) # Or await parse_mcp(live_url)
#     # assert output is not None
#     # assert "MCP Server: Tree+ Test" in output[0]
#     # ... more assertions based on live server output
#     pass
```
