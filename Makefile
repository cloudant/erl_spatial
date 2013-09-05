CS_MAP_DIR:=/usr/local/share/CsMap/Dictionaries/

compile:
	@./rebar compile

clean:
	@./rebar clean

test:
	@./rebar eunit skip_deps=true

doc:
	@./rebar doc


