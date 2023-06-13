SHELL := /bin/bash


# DEVELOP with `make debug`
debug: 
	nodemon -L -V

debug_command: cli test

move_powershell_profile:
	cp mnt/c/Users/$(WIN_USERNAME)/Documents/WindowsPowerShell/Microsoft.PowerShell_profile.ps1 ./Microsoft.PowerShell_profile.ps1

cli:
	pip install -e .

test: test_tp_dotdot
	pytest tests/ -k "not test_tree_plus_dotdot" -vv

test_tp_dotdot:
	cd tests/dot_dot/nested_dir && pytest -k test_tree_plus_dotdot -vv

.PHONY: cli test
