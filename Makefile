init:
	conda activate py310

test:
	pytest tests/

run:
	python3 -m tree_plus.cli

.PHONY: init test run
