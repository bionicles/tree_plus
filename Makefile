SHELL := /bin/bash

cli:
	pip install -U -e .[dev]

# DEVELOP with `make debug`
debug: 
	nodemon -L -V

.PHONY: debug_command
debug_command: test

test: test_normally test_tp_dotdot test_e2e test_cli test_deploy

# first we'll do our unit tests (most likely to need fast debug)
test_normally:
	pytest tests/ -k "engine or units or more_languages or dotenv" -vv

# then we have a test where we change directory
test_tp_dotdot:
	cd tests/dot_dot/nested_dir && pytest -k test_tree_plus_dotdot -vv

# then we do e2e tests 
test_e2e:
	pytest tests/ -k "e2e" -vv

# then we reinstall the cli and test it
test_cli: cli
	pytest tests/test_cli.py -k "cli" -vv

# finally, we'll test the deployment script
test_deploy:
	pytest tests/test_deploy.py

# vulture helps identify dead code
vulture: install_vulture
	vulture tree_plus_cli.py tree_plus_src tests/*.py tests/dot_dot/nested_dir/test_tp_dotdot.py

install_vulture:
	pip install vulture

build: install-build-tool clean-dist
	python -m build

install-wheel:
	pip install -U dist/*.whl

install-build-tool:
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
	tree_plus -g "*.*s" -i group_todo tests/more_languages

t4:
	tree_plus tests/more_languages/group_todo

t5:
	tree_plus -h