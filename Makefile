SHELL := /bin/bash

cli:
	pip install -U -e .[dev]

library_demo:
	python tree_plus_programs/hello_tree_plus.py

rewrite_demo:
	time tree_plus_programs/rewrite.py -l 3333 tests/more_languages/group1/CUSTOMER-INVOICE.CBL customer_invoice.erl

# benchmark, dual core, library coverage, with line numbers for missing tests
coverage:
	time py.test -n 2 --cov=tree_plus_src --cov-report=term-missing --cov-report=lcov:coverage/lcov.info tests/*.py 

# DEVELOP with `make debug`
debug: 
	nodemon -L -V

.PHONY: debug_command
debug_command: test_parallel

# TESTS
N_WORKERS=12
# parallel unit tests (for dev rig)
test_parallel:
	time (py.test --durations=0 -n $(N_WORKERS) --cov=tree_plus_src --cov-report=term-missing --cov-report=lcov:coverage/lcov.info -vv tests/test_*.py)

# sequential unit tests (for CI)
test_sequential:p
	pytest tests/test_more_language_units.py tests/test_units.py tests/test_engine.py -vv

# just to crank on language features, easy to debug on this
test_more_languages:
	pytest tests/test_more_language_units.py -vv

test: test_parallel
# test: test_sequential test_tp_dotdot test_e2e test_cli test_programs test_deploy

# first we'll do our unit tests (most likely to need fast debug)
test_units:
	pytest tests/ -k "engine or units or more_languages or dotenv" -vv

# then we have a test where we change directory
test_tp_dotdot:
	cd tests/dot_dot/nested_dir && pytest -k test_tree_plus_dotdot -vv

# then we do e2e tests 
test_e2e:
	pytest tests/test_e2e.py -k "e2e" -vv

# then we reinstall the cli and test it
test_cli: cli
	pytest tests/test_cli.py -k "cli" -vv

# make sure the library usage examples and examples work
test_programs:
	pytest tests/test_programs.py -vv

# finally, we'll test the deployment script
test_deploy:
	pytest tests/test_deploy.py

# VULTURE helps identify dead code
vulture: install_vulture
	vulture tree_plus_cli.py tree_plus_src tests/*.py tests/dot_dot/nested_dir/test_tp_dotdot.py tree_plus_programs/*.py

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