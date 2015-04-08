%% @doc This module defines a behavior that needs to be implemented using a
%%      specific JSON encoding/decoding library.
%%      The module implementing the behavior that will be used is determined
%%      by this application's `json` configuration value.
-module(egithub_json).

%% API
-export([
         encode/1,
         decode/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Behavior definition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type json() ::
  binary() | number() | null | boolean() |
  #{binary()|atom() => json()} | [json()].

-callback encode(json()) -> binary().
-callback decode(iodata()) -> json().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec encode(json()) -> binary().
encode(Map) ->
  Module = json_module(),
  Module:encode(Map).

-spec decode(iodata()) -> json().
decode(Map) ->
  Module = json_module(),
  Module:decode(Map).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec json_module() -> atom().
json_module() ->
  application:get_env(egithub, json, egithub_jiffy).
