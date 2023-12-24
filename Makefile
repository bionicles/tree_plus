SHELL := /bin/bash


# DEVELOP with `make debug`
debug: 
	nodemon -L -V

.PHONY: debug_command
debug_command: cli test

cli:
	pip install -e .[dev]

test_s: test_tp_dotdot_s
	pytest tests/ -s -k "not test_tree_plus_dotdot" -vv
test_tp_dotdot_s: test_tp_dotdot
	pytest tests/ -s -k "not test_tree_plus_dotdot" -vv

test: test_tp_dotdot
	pytest tests/ -k "not test_tree_plus_dotdot" -vv

test_dotenv:
	pytest tests/ -k "dotenv"

test_tp_dotdot:
	cd tests/dot_dot/nested_dir && pytest -k test_tree_plus_dotdot -vv

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