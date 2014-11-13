-module(egithub_json_SUITE).

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1
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

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(_Config) ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_callback(_Config) ->
  application:set_env(json, egithub, egithub_json_example),
  1 = egithub_json:encode(#{}).

decode_callback(_Config) ->
  application:set_env(json, egithub, egithub_json_example),
  2 = egithub_json:decode(#{}).
