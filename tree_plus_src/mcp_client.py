import aiohttp
import asyncio
from typing import List, Dict, Any, Optional
from urllib.parse import urljoin
# Assuming debug_print is in tree_plus_src.debug
# Add a fallback if tree_plus_src.debug is not found during direct execution
try:
    from .debug import debug_print
except ImportError:
    # Fallback for direct execution (e.g. python tree_plus_src/mcp_client.py)
    def debug_print(*args, **kwargs):
        print("DEBUG:", *args, **kwargs)

async def fetch_capabilities(session: aiohttp.ClientSession, base_url: str) -> Optional[Dict[str, Any]]:
    """Fetches capabilities from the MCP server."""
    # Ensure base_url ends with a slash if mcp_path is relative
    if not base_url.endswith('/'):
        base_url += '/'
    capabilities_url = urljoin(base_url, "capabilities") # capabilities is usually a fixed path relative to mcp_path
    debug_print(f"Attempting to fetch capabilities from: {capabilities_url}")
    try:
        async with session.get(capabilities_url) as response:
            response.raise_for_status()  # Raise an exception for HTTP errors
            return await response.json()
    except aiohttp.ClientResponseError as e:
        debug_print(f"HTTP error fetching capabilities from {capabilities_url}: {e.status} {e.message}")
        return None
    except aiohttp.ClientError as e:
        debug_print(f"ClientError fetching capabilities from {capabilities_url}: {e}")
        return None
    except Exception as e:
        debug_print(f"An unexpected error occurred when fetching capabilities from {capabilities_url}: {type(e).__name__} - {e}")
        return None

def format_arg(arg_name: str, arg_info: Dict[str, Any]) -> str:
    """Formats a single argument for display."""
    # Pydantic V2 uses 'anyOf' for optional fields with None default, and 'type' for others.
    # It also uses 'title' for the actual type name if it's a Pydantic model.
    
    arg_type_str = "Any"
    if 'anyOf' in arg_info: # Optional field in Pydantic v2 often has anyOf
        type_info = next((t for t in arg_info['anyOf'] if t.get('type') != 'null'), None)
        if type_info:
             arg_type_str = type_info.get('title', type_info.get('type', 'Any'))
        else: # Fallback if only null is in anyOf (should not happen for proper optional)
            arg_type_str = arg_info['anyOf'][0].get('title', arg_info['anyOf'][0].get('type', 'Any'))
    elif 'type' in arg_info:
        arg_type_str = arg_info.get('title', arg_info['type'])
        if isinstance(arg_type_str, list): # Handle cases like "type": ["string", "null"]
            arg_type_str = " | ".join(t for t in arg_type_str if t != "null")

    default_value = arg_info.get('default')
    description = arg_info.get('description')

    formatted_arg = f"{arg_name}: {arg_type_str}"
    if default_value is not None:
        formatted_arg += f" = {default_value!r}"
    if description:
        formatted_arg += f" - '{description}'" # Ensure description is quoted
    return formatted_arg

def format_tool_signature(tool_name: str, tool_info: Dict[str, Any]) -> str:
    """Formats the tool signature string."""
    args_schema = tool_info.get('args_schema', {})
    properties = args_schema.get('properties', {})
    required_args_list = args_schema.get('required', [])
    
    sorted_arg_names = []
    optional_arg_names = []

    for arg_name in properties.keys():
        if arg_name in required_args_list:
            sorted_arg_names.append(arg_name)
        else:
            optional_arg_names.append(arg_name)
    
    optional_arg_names.sort() # Sort optional arguments alphabetically
    sorted_arg_names.extend(optional_arg_names) # Required args first, then sorted optionals

    formatted_args = [format_arg(arg_name, properties[arg_name]) for arg_name in sorted_arg_names]
    args_string = ", ".join(formatted_args)
    
    output_type_info = tool_info.get('output_type', {})
    output_type_str = "Any"
    if isinstance(output_type_info, dict):
        # Similar logic to argument type formatting for Pydantic v2
        if 'anyOf' in output_type_info:
            type_info = next((t for t in output_type_info['anyOf'] if t.get('type') != 'null'), None)
            if type_info:
                output_type_str = type_info.get('title', type_info.get('type', 'Any'))
        elif 'type' in output_type_info:
            output_type_str = output_type_info.get('title', output_type_info['type'])
        elif output_type_info == {}: # No output type specified
             output_type_str = "None" 
    elif isinstance(output_type_info, str): # Direct string type
        output_type_str = output_type_info if output_type_info else "Any"


    return f"{tool_name}({args_string}) -> {output_type_str}"

async def parse_mcp_capabilities(capabilities_data: Dict[str, Any], server_url: str) -> List[str]:
    """Parses MCP capabilities into a list of strings for tree_plus."""
    output = []

    server_name = capabilities_data.get('name', 'Unknown MCP Server')
    server_description = capabilities_data.get('description', '') # Default to empty if not present
    
    output.append(f"MCP Server: {server_name} ({server_url})")
    if server_description: # Only add if description is not empty
        output.append(f"  Description: {server_description}")

    # Tools
    tools = capabilities_data.get('tools', [])
    if tools:
        output.append("Tools:")
        for tool_info in sorted(tools, key=lambda t: t.get('name', '')):
            tool_name = tool_info.get('name', 'Unnamed Tool')
            tool_description = tool_info.get('description', '')
            
            signature = format_tool_signature(tool_name, tool_info)
            output.append(f"  Tool: {signature}")
            if tool_description:
                output.append(f"    Description: {tool_description}")

    # Resources
    resources = capabilities_data.get('resources', [])
    if resources:
        output.append("Resources:")
        for resource_info in sorted(resources, key=lambda r: r.get('path', '')):
            resource_path = resource_info.get('path', 'No path')
            resource_description = resource_info.get('description', '')
            output.append(f"  Resource: {resource_path}")
            if resource_description:
                output.append(f"    Description: {resource_description}")

    # Prompts
    prompts = capabilities_data.get('prompts', [])
    if prompts:
        output.append("Prompts:")
        for prompt_info in sorted(prompts, key=lambda p: p.get('name', '')):
            prompt_name = prompt_info.get('name', 'Unnamed Prompt')
            prompt_description = prompt_info.get('description', '')
            signature = format_tool_signature(prompt_name, prompt_info) # Reusing tool formatter
            output.append(f"  Prompt: {signature}")
            if prompt_description:
                output.append(f"    Description: {prompt_description}")
                
    # Models
    models = capabilities_data.get('models', [])
    if models:
        output.append("Models:")
        for model_info in sorted(models, key=lambda m: m.get('id', m.get('name', ''))): # Use 'id' or 'name'
            model_id = model_info.get('id', model_info.get('name', 'Unnamed Model'))
            # Add more details if available, e.g., description
            model_description = model_info.get('description', '')
            output_str = f"  Model: {model_id}"
            if model_description:
                output_str += f" - {model_description}"
            output.append(output_str)
            
    return output

async def parse_mcp(url: str) -> List[str]:
    """Fetches MCP server capabilities and formats them for tree_plus."""
    async with aiohttp.ClientSession() as session:
        capabilities_data = await fetch_capabilities(session, url)
        if capabilities_data:
            return await parse_mcp_capabilities(capabilities_data, url)
        else:
            return [f"Could not fetch or parse capabilities from {url}."]

def parse_mcp_sync(url: str) -> List[str]:
    """Synchronous wrapper for parse_mcp."""
    try:
        loop = asyncio.get_event_loop_policy().get_event_loop()
        if loop.is_running():
            # This case is tricky. For CLI, it's unlikely.
            # If running in an env with an active loop (e.g. Jupyter),
            # this might need `nest_asyncio` or a different approach.
            # For tree_plus CLI, we assume we can run a new loop or use existing if not running.
            debug_print("Asyncio loop is already running. Creating a new task.")
            future = asyncio.ensure_future(parse_mcp(url))
            # This might not behave as expected if the existing loop is busy.
            # Fallback to creating a new loop if this path proves problematic.
            # For now, let's try to use the running loop to complete the future.
            # This requires the outer context to allow this.
            # A safer bet for library code called from various contexts might be to always run in a new thread/loop.
            # However, for tree_plus CLI, this should generally be fine.
            return loop.run_until_complete(future) # This line can be problematic in some running loops.
        else:
            return loop.run_until_complete(parse_mcp(url))
    except RuntimeError as e:
        debug_print(f"RuntimeError with event loop: {e}. Creating and running a new loop.")
        # This is a common fallback
        new_loop = asyncio.new_event_loop()
        asyncio.set_event_loop(new_loop)
        try:
            result = new_loop.run_until_complete(parse_mcp(url))
        finally:
            new_loop.close()
            asyncio.set_event_loop(None) # Clean up global loop state
        return result
    except Exception as e:
        debug_print(f"Unexpected error in parse_mcp_sync: {type(e).__name__} - {e}")
        return [f"An unexpected error occurred while processing MCP URL {url}: {str(e)}"]

if __name__ == '__main__':
    # Example usage for testing mcp_client.py directly
    # Ensure the test server (tests/servers/demo_server.py) is running:
    # python tests/servers/demo_server.py
    test_url = "http://localhost:5123/mcp/" # Ensure trailing slash if server expects it or adjust urljoin
    
    print(f"--- Synchronous Call to {test_url} ---")
    output_sync = parse_mcp_sync(test_url)
    if output_sync:
        for line in output_sync:
            print(line)
    else:
        print("No output or error in synchronous call.")

    # Async call example (if you want to run this file directly with `python tree_plus_src/mcp_client.py`)
    async def main_async_test():
        print(f"\n--- Asynchronous Call to {test_url} ---")
        output_async = await parse_mcp(test_url)
        if output_async:
            for line in output_async:
                print(line)
        else:
            print("No output or error in asynchronous call.")
            
    # To run the async main_async_test:
    # asyncio.run(main_async_test()) # Python 3.7+
    # Or for manual loop management:
    # current_loop = asyncio.get_event_loop()
    # current_loop.run_until_complete(main_async_test())
