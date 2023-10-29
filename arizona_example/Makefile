.PHONY: daemon

# Useful to test the server during development.
daemon:
	ENV="dev" rebar3 as dev shell --eval "sync:go()."
