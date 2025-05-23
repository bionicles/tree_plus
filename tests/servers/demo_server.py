print("DEMO_SERVER: Script start")
import sys

print(f"DEMO_SERVER: Python version: {sys.version}")
print("DEMO_SERVER: Importing FastMCP...")
from mcp.server.fastmcp import FastMCP

print("DEMO_SERVER: FastMCP imported.")

print("DEMO_SERVER: Initializing FastMCP server object...")
mcp = FastMCP("Tree+ Test", host="0.0.0.0", port=5123, mcp_path="/mcp")
print("DEMO_SERVER: FastMCP server object initialized.")


@mcp.resource("config://app")
def config() -> str:
    print("DEMO_SERVER: config resource called")
    return "demo-config"


@mcp.resource("greeting://{name}")
def greeting(name: str) -> str:
    print(f"DEMO_SERVER: greeting resource called with name: {name}")
    return f"Hello, {name}"


@mcp.tool()
def add(a: int, b: int) -> int:
    print(f"DEMO_SERVER: add tool called with a={a}, b={b}")
    return a + b


@mcp.tool()
async def echo(txt: str) -> str:
    print(f"DEMO_SERVER: echo tool called with txt: {txt}")
    return txt


@mcp.prompt("uno", "dos")
def review(code: str) -> str:
    """annotation"""
    print(f"DEMO_SERVER: review prompt called with code: {code}")
    return f"Review this:\n\n{code}"


if __name__ == "__main__":
    print("DEMO_SERVER: In __main__ block.")
    # Using transport="stateless-http" as per SDK docs for stateless_http=True
    # Also providing a default mcp_path, host, and port as these are often needed.
    # If these also cause issues, they can be removed in a subsequent step for further minimalism.
    print(
        "DEMO_SERVER: Attempting mcp.run(transport='stateless-http', host='0.0.0.0', port=5123, mcp_path='/mcp')"
    )
    try:
        mcp.run("streamable-http")
        print(
            "DEMO_SERVER: mcp.run() completed (this line might not be reached if server runs indefinitely)."
        )
    except Exception as e:
        print(f"DEMO_SERVER: Exception during mcp.run(): {type(e).__name__} - {e}")
        # import traceback # Consider adding if not already present and we get here
        # traceback.print_exc()
        raise
