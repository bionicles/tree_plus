import subprocess
import os
import sys
import pytest
from pathlib import Path

# Helper to get tree_plus version for messages
from tree_plus_src import __version__

# Define the location of the expected output file
CURRENT_DIR = Path(__file__).parent
EXPECTED_OUTPUT_FILE = CURRENT_DIR / "expected_mcp_output.txt"
MCP_SERVER_URL = "http://localhost:5123/mcp/" # Ensure trailing slash for consistency, client handles stripping

@pytest.fixture(scope="module", autouse=True)
def manage_mcp_server():
    """Starts the MCP test server before tests in this module and stops it after."""
    print("\nStarting MCP test server...")
    start_command = "make mcp-test-server"
    try:
        # The Makefile target 'mcp-test-server' is responsible for waiting until the server is ready.
        server_process = subprocess.Popen(start_command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        print("MCP test server start initiated by Makefile target.")
        # Allow Popen to complete, assuming 'make' blocks until server is ready or backgrounds.
        # If 'make' backgrounds immediately, this Popen might exit too soon.
        # The 'until curl' in the Makefile target is key here.
        # A timeout could be added to server_process.wait(timeout=XX) if needed,
        # but the make target should handle readiness.
    except Exception as e:
        pytest.fail(f"Failed to start MCP server with command '{start_command}': {e}")

    yield # This is where the testing happens

    print("\nStopping MCP test server...")
    stop_command = "make stop-mcp-test-server"
    try:
        stop_result = subprocess.run(stop_command, shell=True, check=True, capture_output=True, text=True, timeout=10)
        print(f"MCP test server stopped by Makefile target. Output:\n{stop_result.stdout}")
        if stop_result.stderr:
            print(f"Stderr from stop command:\n{stop_result.stderr}")
    except subprocess.CalledProcessError as e:
        print(f"Warning: Failed to stop MCP server cleanly with command '{stop_command}'. Output:\n{e.stdout}\nStderr:\n{e.stderr}")
    except subprocess.TimeoutExpired as e:
        print(f"Warning: Timeout stopping MCP server with command '{stop_command}'. Output:\n{e.stdout}\nStderr:\n{e.stderr}")
    except Exception as e:
        print(f"Warning: Exception while stopping MCP server with command '{stop_command}': {e}")

def test_tree_plus_mcp_cli_concise_output(manage_mcp_server):
    """
    Tests the tree_plus CLI with an MCP server URL in concise mode.
    Compares the beginning of the output with the expected server line.
    """
    try:
        result = subprocess.run(
            [sys.executable, "-m", "tree_plus_cli", MCP_SERVER_URL, "-c", "-d"], 
            capture_output=True,
            text=True,
            check=True,
            timeout=30 
        )
    except subprocess.CalledProcessError as e:
        print("STDOUT:", e.stdout)
        print("STDERR:", e.stderr)
        pytest.fail(f"tree_plus CLI command failed: {e}")
    except subprocess.TimeoutExpired as e:
        print("STDOUT:", e.stdout)
        print("STDERR:", e.stderr)
        pytest.fail(f"tree_plus CLI command timed out: {e}")
    except FileNotFoundError:
        pytest.fail("Failed to find tree_plus_cli.py or python executable. Ensure it's in PATH or installed.")

    stdout = result.stdout.strip()
    print("\n--- Actual tree_plus output (concise) ---")
    print(stdout)
    print("--- End of actual output ---")

    try:
        with open(EXPECTED_OUTPUT_FILE, "r", encoding="utf-8") as f:
            expected_server_line = f.readline().strip()
    except FileNotFoundError:
        pytest.fail(f"Expected output file not found: {EXPECTED_OUTPUT_FILE}")
    
    assert expected_server_line in stdout, \
        f"Expected server line '{expected_server_line}' not found in the CLI output.\nOutput was:\n{stdout}"

def test_tree_plus_mcp_cli_full_output(manage_mcp_server):
    """
    Tests the tree_plus CLI with an MCP server URL in full (non-concise) mode.
    Checks for key elements that should be present.
    """
    try:
        result = subprocess.run(
            [sys.executable, "-m", "tree_plus_cli", MCP_SERVER_URL, "-d"],
            capture_output=True,
            text=True,
            check=True,
            timeout=30
        )
    except subprocess.CalledProcessError as e:
        print("STDOUT:", e.stdout)
        print("STDERR:", e.stderr)
        pytest.fail(f"tree_plus CLI command failed: {e}")
    except subprocess.TimeoutExpired as e:
        print("STDOUT:", e.stdout)
        print("STDERR:", e.stderr)
        pytest.fail(f"tree_plus CLI command timed out: {e}")
    except FileNotFoundError:
        pytest.fail("Failed to find tree_plus_cli.py or python executable.")

    stdout = result.stdout.strip()
    print("\n--- Actual tree_plus output (full) ---")
    print(stdout)
    print("--- End of actual output ---")

    expected_mcp_url_display = MCP_SERVER_URL.rstrip('/')
    assert f"MCP Server: Tree+ Test ({expected_mcp_url_display})" in stdout
    assert "Description: A test server for tree_plus using FastMCP." in stdout

    assert "Tools:" in stdout
    assert "Tool: add(a: integer - 'The first integer.', b: integer - 'The second integer.') -> integer" in stdout
    assert "    Description: Adds two integers." in stdout 
    assert "Tool: echo(txt: string - 'The text to echo.') -> string" in stdout

    assert "Resources:" in stdout
    assert "Resource: config://app" in stdout
    assert "    Description: Returns the application demo config." in stdout # Added from demo_server
    assert "Resource: greeting://{name}" in stdout
    assert "    Description: Greets the given name." in stdout # Added from demo_server
    
    assert "Prompts:" in stdout
    assert "Prompt: review(code: string - 'The code to review.') -> string" in stdout
    assert "    Description: Reviews the given code." in stdout # Added from demo_server


    assert "tree_plus v" in stdout 
    assert f"paths=('{MCP_SERVER_URL}',)" in stdout 
    assert "token(s) in" in stdout
