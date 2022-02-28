all: dialyzer test

openapi:
	QUIET=1 rebar3 openapi generate

dialyzer:
	QUIET=1 rebar3 dialyzer

build:
	QUIET=1 rebar3 compile

test:
	QUIET=1 rebar3 eunit

cover:
	QUIET=1 rebar3 eunit --cover
	QUIET=1 rebar3 cover

clean:
	$(RM) -r _build

.PHONY: all openapi dialyzer build test cover clean
