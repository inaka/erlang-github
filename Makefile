PROJECT = egithub

DEPS = lager sync jiffy ibrowse

dep_lager = git https://github.com/basho/lager.git 2.0.3
dep_sync = git https://github.com/rustyio/sync.git master
dep_jiffy = git https://github.com/davisp/jiffy 0.11.3
dep_ibrowse = git https://github.com/cmullaparthi/ibrowse v4.1.1

include erlang.mk
