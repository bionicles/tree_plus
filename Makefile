init:
	conda activate py310

test:
	pytest tests/

v-test:
	pytest tests/ -v

vv-test:
	pytest tests/ -vv

run:
	python3 -m tree_plus.cli

cli:
	pip install -e .

.PHONY: init test run
