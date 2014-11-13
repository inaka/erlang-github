-module(egithub_example).

-behaviour(egithub_json).

-export([
         encode/1,
         decode/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% egithub_json callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec encode(map()) -> binary().
encode(_Map) ->
  1.

-spec decode(binary()) -> map().
decode(_Json) ->
  2.
