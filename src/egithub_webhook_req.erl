%%% @doc egithub webhook request module for handling
%%%      the opaque request data type.
%%%      this is used internally by the egithub_webhook module

-module(egithub_webhook_req).

-opaque request() :: #{headers => map(), body => map()|binary()}.
-export_type([request/0]).

%% API
-export([new/2, headers/1, header/2, header/3, payload/1]).

-spec new(map(), binary()|map()) -> request().
new(Headers, Body) -> #{headers => Headers, body => Body}.

-spec headers(request()) -> map().
headers(#{headers := Headers}) -> Headers.

-spec header(binary(), request()) -> binary()|undefined.
header(Header, Request) -> header(Header, Request, undefined).

-spec header(binary(), request(), binary()|undefined) -> binary()|undefined.
header(Header, #{headers := Headers}, Default) ->
  maps:get(Header, Headers, Default).

-spec payload(request()) -> {request(), map()}.
payload(Request=#{body := Body}) when is_binary(Body)->
  with_decoded_body(Request);
payload(Request=#{body := DecodedBody}) when is_map(DecodedBody) ->
  {Request, DecodedBody}.

-spec with_decoded_body(request()) -> {request(), map()}.
with_decoded_body(Request=#{body := Body}) ->
  DecodedBody = egithub_json:decode(Body),
  {Request#{body => DecodedBody}, DecodedBody}.