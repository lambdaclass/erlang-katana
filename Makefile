PROJECT = katana

DEPS = sync eper aleppo
TEST_DEPS = xref_runner

dep_eper = git https://github.com/massemanet/eper.git 0.94.0
dep_sync = git https://github.com/inaka/sync.git 0.1.3
dep_aleppo = git https://github.com/inaka/aleppo 0.9.2
dep_xref_runner = git https://github.com/inaka/xref_runner.git 0.2.2

include erlang.mk

COMPILE_FIRST = ktn_recipe

CT_OPTS = -cover test/katana.coverspec

shell: app
	erl -name ${PROJECT}@`hostname` -pa ebin -pa deps/*/ebin -s sync
