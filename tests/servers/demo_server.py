from mcp.server.fastmcp import FastMCP

mcp = FastMCP("Tree+ Test", stateless_http=True)

@mcp.resource("config://app")
def config() -> str: return "demo-config"

@mcp.resource("greeting://{name}")
def greeting(name: str) -> str: return f"Hello, {name}"

@mcp.tool()
def add(a: int, b: int) -> int: return a + b

@mcp.tool()
async def echo(txt: str) -> str: return txt

@mcp.prompt()
def review(code: str) -> str:
    return f"Review this:\n\n{code}"

if __name__ == "__main__":
    mcp.run(transport="streamable-http", host="0.0.0.0", port=5123, mcp_path="/mcp")
