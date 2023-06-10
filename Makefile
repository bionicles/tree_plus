init:
	conda activate py310

test: test_tp_dotdot
	pytest tests/ -k "not test_tree_plus_dotdot" -vv

test_tp_dotdot:
	cd tests/dot_dot/nested_dir && pytest -k test_tree_plus_dotdot -vv

run:
	python3 -m tree_plus.cli

cli:
	pip install -e .

.PHONY: init test run
