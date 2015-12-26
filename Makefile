.PHONY: compile clean dev upgrade

compile:
	./rebar3 compile

upgrade:
	./rebar3 upgrade

clean:
	./rebar3 clean

dialyzer:
	./rebar3 dialyzer

shell:
	erl -pa _build/default/lib/*/ebin \
	-config config/sys \
	-eval "application:ensure_all_started(talks_tweeter)"

rebar3:
	wget -c https://s3.amazonaws.com/rebar3/rebar3
	chmod +x rebar3

