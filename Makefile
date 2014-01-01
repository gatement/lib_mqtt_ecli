#makefile

REBAR = C:/dev/rebar/rebar.cmd


main:
	$(REBAR) get-deps compile


.PHONY: test clean

test:
	$(REBAR) eunit

clean:
	rm -rf ./ebin
