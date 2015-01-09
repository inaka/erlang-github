-module(egithub_webhook).

-export([event/3]).

-export_type([request/0]).

-type event() :: pull_request.
-type request() :: #{headers => map(), body => map()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type messge() :: #{commit_id => string(),
                    path      => binary(),
                    position  => pos_integer(),
                    text      => binary()
                    }.
-type file() :: map().
-export_type([messge/0, file/0]).

-callback handle_pull_request(egithub:credentials(), string(), [map()]) ->
            {ok, [messge()]} | {error, term()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec event(atom(), egithub:credentials(), request()) -> ok | {error, term()}.
event(Module, Cred, #{headers := Headers, body := Body}) ->
  case maps:get(<<"x-github-event">>, Headers, undefined) of
    undefined ->
      {error, missing_header};
    EventName ->
      EventData = jiffy:decode(Body, [return_maps]),
      event(Module, Cred, EventName, EventData)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Events

-spec event(atom(), egithub:credentials(), event(), map()) ->
        ok | {error, term()}.
event(Module, Cred, <<"pull_request">>,
      #{<<"number">> := PR, <<"repository">> := Repository}) ->
  Repo = binary_to_list(maps:get(<<"full_name">>, Repository)),
  {ok, GithubFiles} = egithub:pull_req_files(Cred, Repo, PR),
  case Module:handle_pull_request(Cred, Repo, GithubFiles) of
    {ok, Messages} ->
      {ok, Comments} = egithub:pull_req_comments(Cred, Repo, PR),
      write_comments(Cred, Repo, PR, Comments, Messages);
    {error, Reason} ->
      {error, Reason}
  end;
event(_Config, _Cred, <<"ping">>, _Data) ->
  ok;
event(_Config, _Cred, Event, _Data) ->
  {error, io_lib:format("Nothing to do for event: ~p.~n", [Event])}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helper functions

%% @doc Comment files that failed rules.
write_comments(Cred, Repo, PR, Comments, Messages) ->
  Fun =
    fun(#{commit_id := CommitId,
          path      := Path,
          position  := Position,
          text      := Text
         }) ->
      write_comment(Cred, Repo, PR, CommitId, Path, Position, Text, Comments)
    end,
  lists:foreach(Fun, Messages).

write_comment(Cred, Repo, PR, CommitId, Path, Position, Text, Comments) ->
  case comment_exists(Comments, Path, Position, Text) of
    exists ->
      Args = [Text, Path, Position],
      lager:info("Comment '~p' for ~p on position ~p is already there", Args);
    not_exists ->
      {ok, _} =
          egithub:pull_req_comment_line(
            Cred, Repo, PR, CommitId, Path, Position, Text
          )
  end.

comment_exists(Comments, Path, Position, Body) ->
  MatchingComments =
    [Comment
     || #{<<"path">>      := CPath,
          <<"position">>  := CPosition,
          <<"body">>      := CBody} = Comment <- Comments,
        CPath == Path, CPosition == Position, CBody == Body],
  case MatchingComments of
    [] -> not_exists;
    [_|_] -> exists
  end.
