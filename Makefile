PROJECT = katana

DEPS = inaka_aleppo xref_runner elvis_core
TEST_DEPS = mixer
SHELL_DEPS = sync
BUILD_DEPS = inaka_mk hexer_mk
LOCAL_DEPS = xmerl tools compiler syntax_tools common_test inets ssl test_server hipe public_key dialyzer wx

# Prevents erlang.mk from downloading elvis_core's katana dependency
IGNORE_DEPS = katana

dep_inaka_aleppo = hex 0.9.5
dep_xref_runner  = hex 0.2.4
dep_mixer 			 = git https://github.com/inaka/mixer.git 0.1.4
dep_elvis_core   = git https://github.com/inaka/elvis_core.git  0.2.6-alpha2
dep_sync         = git https://github.com/rustyio/sync.git 9c78e7b
dep_inaka_mk     = git https://github.com/inaka/inaka.mk.git 1.0.0
dep_hexer_mk     = git https://github.com/inaka/hexer.mk.git 1.0.1

DEP_PLUGINS = inaka_mk hexer_mk

include erlang.mk

COMPILE_FIRST = ktn_recipe

ERLC_OPTS := +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

TEST_ERLC_OPTS += +debug_info
CT_OPTS = -cover test/cover.spec

SHELL_OPTS = -s sync

erldocs:
	erldocs . -o docs
