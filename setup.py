# tree_plus/setup.py
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

# TODO: automatically install tree-sitter-{language} grammars
# import subprocess
# import platform
# from setuptools import setup
# from setuptools.command.install import install


# class CustomInstall(install):
#     def run(self):
#         try:
#             make_exe = "make"
#             # Check if on Windows
#             if platform.system() == "Windows":
#                 make_exe = "nmake"

#             # Clone tree-sitter and tree-sitter-csharp grammars
#             subprocess.check_call(
#                 [
#                     "git",
#                     "clone",
#                     "--depth",
#                     "1",
#                     "https://github.com/tree-sitter/tree-sitter-c-sharp.git",
#                 ]
#             )

#             # Build tree-sitter-csharp
#             subprocess.check_call([make_exe], cwd="tree-sitter-c-sharp")

#         except subprocess.CalledProcessError as error:
#             print(
#                 "Error occurred while building tree-sitter or its grammars:",
#                 error.output,
#             )
#             raise error

#         super().run()


# setup(
#     name="tree_plus",
#     version="0.1",
#     py_modules=["tree_plus_cli"],
#     packages=["tree_plus_src"],
#     install_requires=["tree_sitter"],
#     entry_points={
#         "console_scripts": [
#             "tree_plus=tree_plus_cli:main",
#         ],
#     },
#     cmdclass={
#         "install": CustomInstall,
#     },
# )
