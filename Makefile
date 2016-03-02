PROJECT = egithub

DEPS = goldrush lager jiffy shotgun
TEST_DEPS = katana inaka_mixer meck
SHELL_DEPS = sync
BUILD_DEPS = inaka_mk hexer_mk

# TODO: Remove this line once https://github.com/ninenines/erlang.mk/issues/473 is fixed
dep_goldrush = hex 0.1.7
dep_lager = hex 3.0.2
dep_jiffy = hex 0.14.7
dep_shotgun = hex 0.2.3
dep_katana = hex 0.2.20
dep_inaka_mixer = hex 0.1.5
dep_meck = hex 0.8.4
dep_sync = git https://github.com/rustyio/sync.git 9c78e7b
dep_inaka_mk = git https://github.com/inaka/inaka.mk.git 1.0.0
dep_hexer_mk = git https://github.com/inaka/hexer.mk.git 1.0.2

DEP_PLUGINS = inaka_mk hexer_mk

include erlang.mk

ERLC_OPTS := +'{parse_transform, lager_transform}'
ERLC_OPTS += +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

TEST_ERLC_OPTS += +'{parse_transform, lager_transform}' +debug_info

COMPILE_FIRST += egithub_json

SHELL_OPTS= -name ${PROJECT}@`hostname` -s egithub -s sync
CT_OPTS = -cover test/cover.spec

erldocs:
	erldocs -o docs/ .
