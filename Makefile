PROJECT = katana

DEPS = sync

dep_sync = git https://github.com/rustyio/sync.git master

include erlang.mk

CT_SUITES = ktn_maps ktn_recipe ktn_numbers

shell: app
	erl -pa ebin -pa deps/*/ebin -s sync
