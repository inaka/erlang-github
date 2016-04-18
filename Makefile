PROJECT = egithub

DEPS = goldrush lager jiffy hackney
TEST_DEPS = katana_test inaka_mixer meck
SHELL_DEPS = sync
BUILD_DEPS = inaka_mk hexer_mk

# TODO: Remove this line once https://github.com/ninenines/erlang.mk/issues/473 is fixed
dep_goldrush = hex 0.1.7
dep_lager = hex 3.0.2
dep_jiffy = hex 0.14.7
dep_hackney = git https://github.com/benoitc/hackney 1.6.0
dep_katana_test = git https://github.com/inaka/katana-test.git 0.0.3
dep_inaka_mixer = hex 0.1.5
dep_meck = hex 0.8.4
dep_sync = git https://github.com/rustyio/sync.git de3c42d
dep_inaka_mk = git https://github.com/inaka/inaka.mk.git 1.0.0
dep_hexer_mk = git https://github.com/inaka/hexer.mk.git 1.1.0

DEP_PLUGINS = inaka_mk hexer_mk

include erlang.mk

SHELL_OPTS= -name ${PROJECT}@`hostname` -s egithub -s sync
CT_OPTS = -cover test/cover.spec

ERLC_OPTS += +'{parse_transform, lager_transform}'
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

erldocs:
	erldocs -o docs/ .
