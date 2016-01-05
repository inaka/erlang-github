PROJECT = egithub

DEPS = lager jiffy ibrowse
TEST_DEPS = katana mixer meck
SHELL_DEPS = sync
BUILD_DEPS = hexer_mk

dep_lager = git https://github.com/basho/lager.git 3.0.2
dep_jiffy = git https://github.com/davisp/jiffy.git 0.14.5
dep_ibrowse = hex 4.2.2
dep_katana = hex 0.2.18
dep_mixer = git https://github.com/inaka/mixer.git 0.1.4
dep_meck = hex 0.8.4
dep_sync = git https://github.com/rustyio/sync.git 9c78e7b
dep_hexer_mk = git https://github.com/inaka/hexer.mk.git 1.0.0

DEP_PLUGINS = hexer_mk

DIALYZER_DIRS := ebin/
DIALYZER_OPTS := --verbose --statistics -Wunmatched_returns

ERLC_OPTS := +'{parse_transform, lager_transform}'
ERLC_OPTS += +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

TEST_ERLC_OPTS += +'{parse_transform, lager_transform}' +debug_info

COMPILE_FIRST += egithub_json

include erlang.mk

SHELL_OPTS= -name ${PROJECT}@`hostname` -s egithub -s sync
CT_OPTS = -cover test/egithub.coverspec

erldocs:
	erldocs -o docs/ .

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
