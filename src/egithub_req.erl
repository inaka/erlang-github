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

-spec create_table() -> ets:tab().
create_table() -> ets:new(?MODULE, [set, named_table, public, {keypos, 2}]).

-spec run(req()) -> {ok, string()} | {error, tuple()}.
run(#req{} = Req) ->
  #req{ uri     = Uri
      , headers = Headers
      , method  = Method
      , body    = Body
      } = Req,
  do_run(Uri, Headers, Method, Body).

-spec run(egithub:credentials(), string()) ->
  string() | {error, term()}.
run(Cred, Uri) ->
  run(Cred, Uri, get, []).

-spec run(egithub:credentials(), iodata(), method(), iodata()) ->
  {ok, string()} | {error, term()}.
run(Cred, Uri, Method, Body) ->
  Headers0 = [{<<"User-Agent">>, <<"Egithub-Webhook">>}],
  Headers  = maps:from_list(authorization(Cred, Headers0)),
  do_run(Uri, Headers, Method, Body).

do_run(Uri, Headers, Method, Body) ->
  {ok, Pid} = shotgun:open("api.github.com", 443, https),
  _ = lager:info("[Github API] ~s", [Uri]),
  try shotgun:request(Pid, Method, Uri, Headers, Body, #{}) of
    {ok, #{status_code := 200, body := RespBody}} ->
      {ok, RespBody};
    {ok, #{status_code := 201, body := RespBody}} ->
      {ok, RespBody};
    {ok, #{status_code := 204, body := RespBody}} ->
      {ok, RespBody};
    {ok, #{status_code := 302, headers := RespHeaders}} ->
      RedirectUrl = proplists:get_value(<<"Location">>, RespHeaders),
      run(RedirectUrl, Headers, Method, Body);
    {ok, #{status_code := Status, headers := RespHeaders, body := RespBody}} ->
      _ = lager:warning(
        "[Github API] Error:~nUri: ~s~nError: ~p~n",
        [Uri, {Status, RespHeaders, RespBody}]),
      {error, {Status, RespHeaders, RespBody}}
  after
    shotgun:close(Pid)
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
    [{basic_auth, {Username, Password}} | Headers];
authorization({oauth, Token}, Headers0) ->
    [{<<"Authorization">>, iolist_to_binary(["token ", Token])} | Headers0].
