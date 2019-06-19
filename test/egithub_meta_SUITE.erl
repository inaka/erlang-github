-module(egithub_meta_SUITE).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ ktn_meta_SUITE
        , [ all/0
          , xref/1
          , dialyzer/1
          , elvis/1
          ]
        }]).

-export([init_per_suite/1, end_per_suite/1]).

-type config() :: [{atom(), term()}].

%% Specifications for functions mixed in from ktn_meta_SUITE
-spec all() -> config().
-spec xref(config()) -> config().
-spec dialyzer(config()) -> config().
-spec elvis(config()) -> config().

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  [ {application,  egithub}
  %% Until the next version of katana-test fixes the missing test deps in plt
  %% issue, we can't use the default warnings that include 'unknown' here.
  , {dialyzer_warnings, [error_handling, race_conditions, unmatched_returns]}
  | Config
  ].

-spec end_per_suite(config()) -> config().
end_per_suite(Config) -> Config.
