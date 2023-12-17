include dotenv/dev.env
export

# # ARIS ENV VARS
# export HAX_PATH=$HOME/hax
# export PROMO_PATH=$HAX_PATH/aris/promo
# alias cdp="cd ${PROMO_PATH}"
# alias tpp="tp ${PROMO_PATH}"
# alias fhp="f $PROMO_PATH"

# @ is used here to make output look cleaner 
# @ suppresses echoing of the command which follows
# C-m in tmux is a carriage return (hit enter)
.PHONY: dev
dev:
	@tmux new-session -d -s 'promo-dev'
	@tmux source-file tmux.conf
	@tmux split-window -v -t 'promo-dev' # comment to split horizontally
	# @tmux split-window -h -t 'promo-dev' # uncomment to split horizontally
	@tmux set -t 'promo-dev' status-right "Copy: Select & Right Click | Exit: ctrl-d | Kill: ctrl-k"
	@tmux send-keys -t 'promo-dev:0.0' 'cd $(PROMO_PATH)' C-m 'make tail-logs' C-m
	@tmux send-keys -t 'promo-dev:0.1' 'cd $(PROMO_PATH)' C-m 'pm2 start dev-stack.config.js; bash' C-m
	@tmux send-keys -t 'promo-dev:0.1' 'tmux set-hook -g session-ended "run-shell \"cd $(PROMO_PATH) && make services-down\""' C-m
	@tmux select-pane -t 'promo-dev:0.0'
	@tmux attach-session -t 'promo-dev'

services-down:
	@echo "Stopping all services..."
	@pm2 delete dev-stack.config.js

services-stop: services-down

# guide the dev to install, start, and enable the postgresql server
define CHECK_POSTGRES
	which psql >/dev/null || \
	(echo "\nPostgreSQL not installed. Install with 'sudo apt install postgresql'. Add 'export PATH="/lib/postgresql/14/bin/:\$PATH"'  in ~/.bash_profile" && exit 1)
	pgrep -x "postgres" >/dev/null || \
	(echo "\nPostgreSQL not running. Start it with 'sudo service postgresql start'." && exit 1)
endef

# Picard: DAMAGE REPORT!
# @tree_plus promo-server,promo-app/src/app/register,promo-app/src/app/events,promo-programs,project.txt -I bin/
damage-report:
	@echo "===== DAMAGE REPORT ====="
	@echo "--- Postgres ---"
	@pgrep -x postgres > /dev/null && echo "PostgreSQL is running." || echo "PostgreSQL is not running."
	# @make db-structure
	
	@echo "\n--- Server ---"
	@if pgrep -f 'uvicorn' > /dev/null; then \
		echo "FastAPI servers are running with following details:"; \
		ps -ef | grep 'uvicorn' | grep -v grep; \
		echo "\nFastAPI server info:"; \
		echo "GET http://localhost:8000/.well_known/server_info"; \
		curl -s http://localhost:8000/.well_known/server_info | bat; \
	else \
		echo "No FastAPI server is running."; \
	fi
	
	@echo "\n--- Client ---"
	@if pgrep -f 'ng serve' > /dev/null; then \
		echo "Angular servers are running with following details:"; \
		ps -ef | grep 'ng serve' | grep -v grep; \
	else \
		echo "No Angular server is running."; \
	fi

	@echo "\n--- Tests ---"
	@make test

	@echo "\n--- DevOps ---"
	@terraform show

	@echo "\n--- Cloud ---"
	@echo "Status: Unknown"

	@echo "===== DAMAGE REPORT ====="

tail-logs:
	@echo "===== LOGS ====="
	@pm2 logs

cloud:
	@echo "Managing cloud resources..."
	@cd promo-cloud && $(MAKE) all