-module(egithub_req).

-export([ run/2
        , run/4
        , queue/4
        ]).

-type req() :: #{ url     => iodata()
                , headers => proplists:proplist()
                , method  => ibrowse:method()
                , body    => ibrowse:body()
                , options => proplists:proplist()
                }.
-export_type([req/0]).

-spec run(egithub:credentials(), string()) ->
  string() | {error, term()}.
run(Cred, Url) ->
  run(Cred, Url, get, []).

-spec run(egithub:credentials(), string(), ibrowse:method(), ibrowse:body()) ->
  {ok, string()} | {error, term()}.
run(Cred, Url, Method, Body) ->
  Options0 = [{ssl_options, [{depth, 0}]}],
  Headers0 = [{"User-Agent", "Elvis-Webhook"}],
  {Options, Headers} = authorization(Cred, Options0, Headers0),
  lager:info("[Github API] ~s", [Url]),
  case ibrowse:send_req(Url, Headers, Method, Body, Options) of
    {ok, "200", _RespHeaders, RespBody} ->
      {ok, RespBody};
    {ok, "201", _RespHeaders, RespBody} ->
      {ok, RespBody};
    {ok, "204", _RespHeaders, RespBody} ->
      {ok, RespBody};
    {ok, "302", RespHeaders, _} ->
      RedirectUrl = proplists:get_value("Location", RespHeaders),
      run(Cred, RedirectUrl, Method, Body);
    {ok, Status, RespHeaders, RespBody} ->
      {error, {Status, RespHeaders, RespBody}}
  end.

-spec queue(
  egithub:credentials(), string(), ibrowse:method(), ibrowse:body()) ->
    ok.
queue(Cred, Url, Method, Body) ->
  Options0 = [{ssl_options, [{depth, 0}]}],
  Headers0 = [{"User-Agent", "Elvis-Webhook"}],
  {Options, Headers} = authorization(Cred, Options0, Headers0),
  Request =
    #{ url      => Url
     , headers  => Headers
     , method   => Method
     , body     => Body
     , options  => Options
     },
  gen_server:cast(egithub_req_in, Request).

authorization({basic, Username, Password}, Options0, Headers) ->
    Options = [{basic_auth, {Username, Password}} | Options0],
    {Options, Headers};
authorization({oauth, Token}, Options, Headers0) ->
    Headers = [{"Authorization", "token " ++ Token} | Headers0],
    {Options, Headers}.
