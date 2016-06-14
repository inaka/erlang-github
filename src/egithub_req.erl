%% @hidden
-module(egithub_req).

-export([ run/1
        , run/2
        , run/4
        , queue/4
        , create_table/0
        ]).

-type method() :: get | post | put | delete.

-record(req, { id = os:timestamp() :: erlang:timestamp()
             , uri                 :: string()
             , headers             :: proplists:proplist()
             , method              :: method()
             , body                :: iodata()
             }).
-type req() :: #req{}.
-export_type([req/0]).

-type error() :: {error, {100..600, [{binary(), binary()}], binary()}}
               | {error, term()}.
-export_type([error/0]).

-spec create_table() -> ets:tab().
create_table() -> ets:new(?MODULE, [set, named_table, public, {keypos, 2}]).

-spec run(req()) -> {ok, string()} | error().
run(#req{} = Req) ->
  #req{ uri     = Uri
      , headers = Headers
      , method  = Method
      , body    = Body
      } = Req,
  do_run(Uri, Headers, Method, Body).

-spec run(egithub:credentials(), string()) ->
  string() | error().
run(Cred, Uri) ->
  run(Cred, Uri, get, []).

-spec run(egithub:credentials(), iodata(), method(), iodata()) ->
  {ok, string()} | error().
run(Cred, Uri, Method, Body) ->
  Headers0 = [{<<"User-Agent">>, <<"Egithub-Webhook">>}],
  Headers  = authorization(Cred, Headers0),
  do_run(Uri, Headers, Method, Body).

do_run(Uri, Headers, Method, Body) ->
  _ = lager:info("[Github API] ~s", [Uri]),
  BinUri = iolist_to_binary(Uri),
  Url = <<"https://api.github.com", BinUri/binary>>,
  case hackney:request(Method, Url, Headers, Body) of
    {ok, 200, _RespHeaders, ClientRef} ->
      hackney:body(ClientRef);
    {ok, 201, _RespHeaders, ClientRef} ->
      hackney:body(ClientRef);
    {ok, 204, _RespHeaders, ClientRef} ->
      hackney:body(ClientRef);
    {ok, 302, RespHeaders, _ClientRef} ->
      RedirectUrl = proplists:get_value(<<"Location">>, RespHeaders),
      do_run(RedirectUrl, Headers, Method, Body);
    {ok, Status, RespHeaders, ClientRef} ->
      {ok, RespBody} = hackney:body(ClientRef),
      _ = lager:warning(
        "[Github API] Error:~nUri: ~s~nError: ~p~n",
        [Uri, {Status, RespHeaders, RespBody}]),
      {error, {Status, RespHeaders, RespBody}};
    {error, Reason} ->
      _ = lager:warning(
        "[Github API] Error:~nUri: ~s~nError: ~p~n", [Uri, Reason]),
      {error, Reason}
  end.

-spec queue(
  egithub:credentials(), string(), method(), iodata()) ->
    ok.
queue(Cred, Uri, Method, Body) ->
  Headers0 = [{<<"User-Agent">>, "Egithub-Webhook"}],
  Headers = authorization(Cred, Headers0),
  Request = #req{ uri     = Uri
                , headers = Headers
                , method  = Method
                , body    = Body
                },
  gen_server:cast(egithub_req_in, Request).

authorization({basic, Username, Password}, Headers) ->
  User1 = list_to_binary(Username),
  Pwd1 = list_to_binary(Password),
  Credentials = base64:encode(<<User1/binary, ":", Pwd1/binary>>),
  [{<<"Authorization">>, <<"Basic ", Credentials/binary>>} | Headers];
authorization({oauth, Token}, Headers0) ->
  [{<<"Authorization">>, iolist_to_binary(["token ", Token])} | Headers0].
