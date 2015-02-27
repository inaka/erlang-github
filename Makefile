PROJECT = egithub

DEPS = lager sync jiffy ibrowse

dep_lager = git https://github.com/basho/lager.git 2.0.3
dep_sync = git https://github.com/rustyio/sync.git master
dep_jiffy = git https://github.com/davisp/jiffy 0.11.3
dep_ibrowse = git https://github.com/cmullaparthi/ibrowse v4.1.1

DIALYZER_DIRS := ebin/
DIALYZER_OPTS := --verbose --statistics -Werror_handling \
                 -Wrace_conditions #-Wunmatched_returns

ERLC_OPTS := +'{parse_transform, lager_transform}' +'{lager_truncation_size, 32768}'
ERLC_OPTS += +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

include erlang.mk

SHELL_OPTS= -name ${PROJECT}@`hostname` -s egithub -s sync

CT_OPTS = -cover test/egithub.coverspec
CT_SUITES = egithub_json
