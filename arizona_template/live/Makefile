.PHONY: serve

# Starts a daemon with hot code reload.
serve:
	ENV="dev" rebar3 as dev shell --eval "sync:go()."
