PROJECT = katana

DEPS = sync aleppo eper

dep_eper = git https://github.com/massemanet/eper.git master
dep_sync = git https://github.com/rustyio/sync.git master
dep_aleppo = git https://github.com/inaka/aleppo 0.9.0

include erlang.mk

COMPILE_FIRST = ktn_recipe

CT_OPTS = -cover test/katana.coverspec

shell: app
	erl -name ${PROJECT}@`hostname` -pa ebin -pa deps/*/ebin -s sync
