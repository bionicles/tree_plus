SHELL := /bin/bash

cli:
	pip install -e .[dev]

# DEVELOP with `make debug`
debug: 
	nodemon -L -V

.PHONY: debug_command
debug_command: test test_cli


# first, test the dotdot stuff (no cli reinstall, for speed)
test: test_tp_dotdot
	pytest tests/ -k "not test_tree_plus_dotdot and not cli" -vv

test_tp_dotdot:
	cd tests/dot_dot/nested_dir && pytest -k test_tree_plus_dotdot -vv

# then, reinstall the cli and test it in there
test_cli: cli
	pytest tests/ -k "cli" -vv


test_dotenv:
	pytest tests/ -k "dotenv"


move_powershell_profile:
	cp mnt/c/Users/$(WIN_USERNAME)/Documents/WindowsPowerShell/Microsoft.PowerShell_profile.ps1 ./Microsoft.PowerShell_profile.ps1

build: install-build clean-dist
	python -m build

install-build:
	pip install --upgrade build

test-publish: install-twine
	python -m twine upload --repository testpypi dist/*

install-twine:
	pip install --upgrade twine

publish: install-twine
	python -m twine upload dist/*

clean-dist:
	rm -rf dist/*

t1:
	tree_plus -i tests

t2:
	tree_plus -i group_todo tests/more_languages

t3:
	tree_plus -g "*.*s" tests/more_languages