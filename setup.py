from setuptools import setup, find_packages

setup(
    name="tree_plus",
    version="0.1",
    packages=find_packages(),
    entry_points={
        "console_scripts": [
            "tree_plus=src.cli:main",
        ],
    },
)
