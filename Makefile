SHELL := /bin/bash


# DEVELOP with `make debug`
debug: 
	nodemon -L -V

.PHONY: debug_command
debug_command: cli test

cli:
	pip install -e .

test_s: test_tp_dotdot_s
	pytest tests/ -s -k "not test_tree_plus_dotdot" -vv
test_tp_dotdot_s: test_tp_dotdot
	pytest tests/ -s -k "not test_tree_plus_dotdot" -vv

test: test_tp_dotdot
	pytest tests/ -k "not test_tree_plus_dotdot" -vv

test_tp_dotdot:
	cd tests/dot_dot/nested_dir && pytest -k test_tree_plus_dotdot -vv

move_powershell_profile:
	cp mnt/c/Users/$(WIN_USERNAME)/Documents/WindowsPowerShell/Microsoft.PowerShell_profile.ps1 ./Microsoft.PowerShell_profile.ps1
