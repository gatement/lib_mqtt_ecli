#makefile

REBAR = C:/dev/rebar/rebar.cmd


main: get-deps co

co: compile

compile:
	$(REBAR) compile

get-deps:
	$(REBAR) get-deps


.PHONY: test clean

test:
	$(REBAR) eunit

cl: clean

clean:
	rm -rf ./ebin
