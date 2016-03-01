%% @doc This module defines a behavior that needs to be implemented using a
%%      specific JSON encoding/decoding library.
%%      The module implementing the behavior that will be used is determined
%%      by this application's <code>json</code> configuration value.
%% @end
-module(egithub_json).

%% API
-export([
         encode/1,
         decode/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Behavior definition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type json() :: binary() | number() | null | boolean() |
                #{binary() | atom() => json()} | [json()].

-callback encode(json()) -> binary().
-callback decode(iodata()) -> json().

-export_type([json/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Encodes the json() value using the specified implementation module
%%      found in the <code>egithub</code> application <code>json</code> env
%%      variable. The default implementation module is
%%      <code>egithub_jiffy</code>.
%% @end
-spec encode(json()) -> binary().
encode(Map) ->
  Module = json_module(),
  Module:encode(Map).

%% @doc Decodes the JSON value as iodata() using the specified implementation
%%      module found in the <code>egithub</code> application <code>json</code>
%%      env variable. The default implementation module is
%%      <code>egithub_jiffy</code>.
%% @end
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
