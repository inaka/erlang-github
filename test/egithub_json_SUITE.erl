-module(egithub_json_SUITE).

-export([
         all/0
        ]).

-export([
         encode_callback/1,
         decode_callback/1
        ]).

-define(EXCLUDED_FUNS,
        [
         module_info,
         all,
         test,
         init_per_suite,
         end_per_suite
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
  Exports = ?MODULE:module_info(exports),
  [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_callback(_Config) ->
  application:unset_env(egithub, json),
  <<"{}">> = egithub_json:encode(#{}),

  application:set_env(egithub, json, egithub_json_example),
  <<"1">> = egithub_json:encode(#{}).

decode_callback(_Config) ->
  application:unset_env(egithub, json),
  #{} = egithub_json:decode(<<"{}">>),

  application:set_env(egithub, json, egithub_json_example),
  2 = egithub_json:decode(<<>>).
