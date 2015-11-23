PROJECT = egithub

DEPS = lager jiffy ibrowse
TEST_DEPS = meck
SHELL_DEPS = sync

dep_lager = git https://github.com/basho/lager.git 3.0.2
dep_sync = git https://github.com/rustyio/sync.git 9c78e7b
dep_jiffy = git https://github.com/davisp/jiffy 0.14.4
dep_ibrowse = git https://github.com/cmullaparthi/ibrowse v4.2
dep_meck = git https://github.com/eproxus/meck 0.8.3

DIALYZER_DIRS := ebin/
DIALYZER_OPTS := --verbose --statistics -Werror_handling \
                 -Wrace_conditions #-Wunmatched_returns

ERLC_OPTS := +'{parse_transform, lager_transform}'
ERLC_OPTS += +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

TEST_ERLC_OPTS += +'{parse_transform, lager_transform}' +debug_info

COMPILE_FIRST += egithub_json

include erlang.mk

SHELL_OPTS= -name ${PROJECT}@`hostname` -s egithub -s sync
CT_OPTS = -cover test/egithub.coverspec -erl_args -config rel/test.config

erldocs:
	erldocs -o docs/ .

quicktest-build:
	@$(MAKE) --no-print-directory app-build test-dir ERLC_OPTS="$(TEST_ERLC_OPTS)"

quicktests: clean-app clean-test-dir quicktest-build
	@if [ -d "test" ] ; \
	then \
		mkdir -p logs/ ; \
		$(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES)) $(CT_OPTS) ; \
	fi
	$(gen_verbose) rm -f test/*.beam
