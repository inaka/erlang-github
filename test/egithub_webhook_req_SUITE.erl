-module(egithub_webhook_req_SUITE).

-include_lib("common_test/include/ct.hrl").
%% API
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

-export([
  get_headers/1,
  encoded_body/1,
  decoded_body/1
]).

-define(EXCLUDED_FUNS,
  [
    module_info,
    all,
    init_per_suite,
    end_per_suite,
    setup_test_data
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
  {ok, _} = egithub:start(),
  application:unset_env(egithub, json),
  [{test_data, setup_test_data()} | Config].

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  ok = application:stop(egithub),
  Config.

setup_test_data() ->
  EncodedBody = <<"{\"foo\":\"bar\"}">>,
  DecodedBody = egithub_json:decode(EncodedBody),
  TestHeaders = #{<<"Accept">> => <<"*">>,
    <<"Content-Type">> => <<"application/json">>},
  EncodedReq = egithub_webhook_req:new(TestHeaders, EncodedBody),
  DecodedReq = egithub_webhook_req:new(TestHeaders, DecodedBody),
  #{
    encoded_body => EncodedBody,
    decoded_body => DecodedBody,
    headers => TestHeaders,
    encoded_request => EncodedReq,
    decoded_request => DecodedReq
  }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_headers(config()) -> binary().
get_headers(Config) ->
  #{
    headers := TestHeaders,
    encoded_request := Req
  } = ?config(test_data, Config),

  TestHeaders = egithub_webhook_req:headers(Req),
  undefined = egithub_webhook_req:header(<<"Undefined">>, Req),
  <<"*">> = egithub_webhook_req:header(<<"Accept">>, Req).

-spec encoded_body(config()) -> {egithub_webhook_req:request(), map()}.
encoded_body(Config) ->
  #{
    encoded_request := Req,
    decoded_body := DecodedBody
  } = ?config(test_data, Config),

  {Req2, DecodedBody} = egithub_webhook_req:payload(Req),
  {_Req3, DecodedBody} = egithub_webhook_req:payload(Req2).

-spec decoded_body(config()) -> {egithub_webhook_req:request(), map()}.
decoded_body(Config) ->
  #{
    decoded_request := Req,
    decoded_body := DecodedBody
  } = ?config(test_data, Config),

  {Req, DecodedBody} = egithub_webhook_req:payload(Req).