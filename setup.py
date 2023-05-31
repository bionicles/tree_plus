from setuptools import setup

setup(
    name="tree_plus",
    version="0.1",
    py_modules=["tree_plus_cli"],
    packages=["tree_plus_src"],
    entry_points={
        "console_scripts": [
            "tree_plus=tree_plus_cli:main",
        ],
    },
)
