PROJECT = katana

DEPS = eper aleppo xref_runner
SHELL_DEPS := sync
TEST_DEPS = mixer
LOCAL_DEPS := xmerl tools compiler syntax_tools common_test inets ssl test_server hipe public_key dialyzer wx

dep_eper = git https://github.com/massemanet/eper.git 0.96.3
dep_sync = git https://github.com/inaka/sync.git 0.1.3
dep_aleppo = git https://github.com/inaka/aleppo.git 0.9.2
dep_xref_runner = git https://github.com/inaka/xref_runner.git 0.2.2
dep_mixer = git https://github.com/inaka/mixer.git 0.1.4

include erlang.mk

COMPILE_FIRST = ktn_recipe

ERLC_OPTS := +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

DIALYZER_DIRS := ebin/ test/
DIALYZER_OPTS := --verbose --statistics -Wunmatched_returns

TEST_ERLC_OPTS += +debug_info
CT_OPTS = -cover test/katana.coverspec

SHELL_OPTS += -name ${PROJECT}@`hostname` -pa ebin -pa deps/*/ebin -s sync

quicktests: app
	@$(MAKE) --no-print-directory app-build test-dir ERLC_OPTS="$(TEST_ERLC_OPTS)"
	$(verbose) mkdir -p $(CURDIR)/logs/
	$(gen_verbose) $(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES)) $(CT_OPTS)

test-build-plt: ERLC_OPTS=$(TEST_ERLC_OPTS)
test-build-plt:
	@$(MAKE) --no-print-directory test-dir ERLC_OPTS="$(TEST_ERLC_OPTS)"
	$(gen_verbose) touch ebin/test

plt-all: PLT_APPS := $(ALL_TEST_DEPS_DIRS)
plt-all: test-deps test-build-plt plt

dialyze-all: app test-build-plt dialyze

erldocs:
	erldocs . -o docs
