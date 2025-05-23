import sys

if sys.version_info.minor > 9:
    import subprocess
    import pytest
    from pathlib import Path

    # Helper to get tree_plus version for messages
    from tree_plus_src import __version__

    # Define the location of the expected output file
    CURRENT_DIR = Path(__file__).parent
    EXPECTED_OUTPUT_FILE = CURRENT_DIR / "expected_mcp_output.txt"
    MCP_SERVER_URL = "http://localhost:5123/mcp/"  # Ensure trailing slash for consistency, client handles stripping

    def test_tree_plus_mcp_cli_full_output():
        """
        Tests the tree_plus CLI with an MCP server URL in full (non-concise) mode.
        Checks for key elements that should be present.
        """
        try:
            result = subprocess.run(
                [sys.executable, "-m", "tree_plus_cli", "--mcp", MCP_SERVER_URL],
                capture_output=True,
                text=True,
                check=True,
                timeout=30,
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

        assert f"Tree+ Test" in stdout

        assert "Tools:" in stdout
        assert "add" in stdout
        assert "echo" in stdout

        assert "Resources:" in stdout
        assert "config" in stdout

        assert "Prompts:" in stdout
        assert 'uno: "dos"' in stdout

        assert "tree_plus v" in stdout
        assert f"paths=('{MCP_SERVER_URL}',)" in stdout
        assert "token(s) in" in stdout
