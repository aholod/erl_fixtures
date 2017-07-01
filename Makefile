compile:
	bin/rebar3 compile

install:
	bin/rebar3 get-deps

test:
	bin/rebar3 eunit

prepare:
	sudo yum install -y libyaml-devel libyaml

prepare-deb:
	sudo apt install libyaml-dev libyaml-0-2

.PHONY: test
