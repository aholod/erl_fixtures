compile:
	bin/rebar3 compile

install:
	bin/rebar3 get-deps

test:
	bin/rebar3 eunit

.PHONY: test
