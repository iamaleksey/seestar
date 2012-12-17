.PHONY: all compile clean doc

all: deps compile doc

deps:
	@./rebar get-deps

compile:
	@./rebar compile

clean:
	@./rebar clean

doc:
	@./rebar skip_deps=true doc
