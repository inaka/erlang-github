%% @doc Implements the code to handle GitHub's webhook events. All callbacks
%%      defined map to a specific GitHub event, for example the
%%      <code>handle_pull_request</code> callback is used for pull_request
%%      events.
%%      It also offers the option of reporting the progress of your webhook
%%      handler through the Statused API functions, by using the
%%      <code>event/6</code> function.
%% @end
-module(egithub_webhook).

-export([event/3, event/6]).

-export_type([request/0]).

-type request() :: #{headers => map(), body => binary()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type message() :: #{commit_id => string(),
                     path      => binary(),
                     position  => pos_integer(),
                     text      => binary()
                    }.
-type file() :: map().
-type req_data() :: map().
-export_type([req_data/0, message/0, file/0]).

-callback handle_pull_request(egithub:credentials(), req_data(), [file()]) ->
  {ok, [message()]} | {error, term()}.

-callback handle_error({error, term()}, req_data(), [file()]) ->
  {error, {failed, integer()}, string()} | {ok, [map()], string()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Should be called from the endpoint that handles the GitHub's request
%%      for the webhook.
%% @end
-spec event(atom(), egithub:credentials(), request()) -> ok | {error, term()}.
event(Module, Cred, #{headers := Headers, body := Body}) ->
  case maps:get(<<"x-github-event">>, Headers, undefined) of
    undefined ->
      {error, missing_header};
    <<"ping">> -> ok;
    <<"pull_request">> ->
      EventData = egithub_json:decode(Body),
      case do_handle_pull_request(Module, Cred, EventData) of
        clean -> ok;
        {clean, _TargetUrl} -> ok;
        with_warnings -> ok;
        {with_warnings, _TargetUrl} -> ok;
        {error, Error} -> {error, Error};
        {error, Error, _TargetUrl} -> {error, Error}
      end;
    EventName -> {error, <<"Unknown event: ", EventName/binary>>}
  end.

%% @doc Should be called from the endpoint that handles the GitHub's request
%%      for the webhook.
%%
%%      The credentials provided in the <code>StatusCred</code> argument need
%%      to have the appropiate permissions to be able to change the
%%      repository's status.
%% @end
-spec event(
  atom(), egithub:credentials(), string(), string(), egithub:credentials(),
  request()) -> ok | {error, term()}.
event(Module, StatusCred, ToolName, Context, CommentsCred, Request) ->
  #{headers := Headers, body := Body} = Request,
  case maps:get(<<"x-github-event">>, Headers, undefined) of
    undefined ->
      {error, missing_header};
    <<"ping">> -> ok;
    <<"pull_request">> ->
      EventData = egithub_json:decode(Body),
      set_status(pending, StatusCred, ToolName, Context, EventData),
      try
        Result = do_handle_pull_request(Module, CommentsCred, EventData),
        specify_status(Result, StatusCred, ToolName, Context, EventData)
      catch
        _:Error ->
          _ = lager:warning(
            "event:Error ~p Module: ~p ToolName: ~p "
            "Context: ~p EventData: ~p get_stacktrace: ~p",
            [ Error
            , Module
            , ToolName
            , Context
            , EventData
            , erlang:get_stacktrace()]),
          ErrSource =
            do_handle_error_source(Error, Module, CommentsCred, EventData),
          _ =
            specify_status(ErrSource, StatusCred, ToolName, Context, EventData),
          throw(Error)
      end;
    EventName -> {error, <<"Unknown event: ", EventName/binary>>}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Statuses
-spec specify_status( atom() | {atom(), any()} | {atom(), any(), string()}
                    , egithub:credentials(), atom(), string()
                    , egithub_json:json()) -> ok | {error, any()}.
specify_status(clean, StatusCred, ToolName, Context, EventData) ->
  set_status(success, StatusCred, ToolName, Context, EventData),
  ok;
specify_status({clean, TargetUrl}, StatusCred, ToolName, Context, EventData) ->
  set_status(success, StatusCred, ToolName, Context, EventData, TargetUrl),
  ok;
specify_status(with_warnings, StatusCred, ToolName, Context, EventData) ->
  set_status(error, StatusCred, ToolName, Context, EventData),
  ok;
specify_status( {with_warnings, TargetUrl}, StatusCred, ToolName, Context
              , EventData) ->
  set_status(error, StatusCred, ToolName, Context, EventData, TargetUrl),
  ok;
specify_status({error, Error}, StatusCred, ToolName, Context, EventData) ->
  set_status({failure, Error}, StatusCred, ToolName, Context, EventData),
  {error, Error};
specify_status( {error, Error, TargetUrl}, StatusCred, ToolName, Context
              , EventData) ->
  set_status( {failure, Error}, StatusCred, ToolName, Context, EventData
            , TargetUrl),
  {error, Error}.

set_status(FullState, StatusCred, ToolName, Context, EventData) ->
  set_status(FullState, StatusCred, ToolName, Context, EventData, undefined).

set_status(FullState, StatusCred, ToolName, Context, EventData, TargetUrl) ->
  #{ <<"repository">> := Repository
   , <<"pull_request">> := #{<<"head">> := Head}
   } = EventData,
  Repo = binary_to_list(maps:get(<<"full_name">>, Repository)),
  Sha = binary_to_list(maps:get(<<"sha">>, Head)),
  Description = status_description(FullState, ToolName),
  State = normalize_state(FullState),
  _ = lager:debug(
    "[Github WH] About to set ~s status in ~s to ~p: ~p",
    [Context, Repo, State, Description]),
    {ok, _} = egithub:create_status(
      StatusCred, Repo, Sha, State, Description, Context, TargetUrl),
  ok.

status_description(pending, ToolName) ->
  ToolName ++ " is checking your pull request";
status_description(success, ToolName) ->
  ToolName ++ " is satisfied with your pull request";
status_description(error, ToolName) ->
  IoData =
    io_lib:format(
      "~s is not happy with your PR, warnings were emitted",
      [ToolName]),
  binary_to_list(iolist_to_binary(IoData));
status_description({failure, Error}, ToolName) ->
  IoData =
    io_lib:format("~s failed while reviewing your PR: `~p`", [ToolName, Error]),
  binary_to_list(iolist_to_binary(IoData)).

normalize_state({State, _}) -> State;
normalize_state(State) -> State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Events

do_handle_pull_request(Module, Cred,
      #{<<"number">> := PR, <<"repository">> := Repository} = Data) ->
  Repo = binary_to_list(maps:get(<<"full_name">>, Repository)),
  {ok, GithubFiles} = egithub:pull_req_files(Cred, Repo, PR),
  case Module:handle_pull_request(Cred, Data, GithubFiles) of
    {ok, []} -> clean;
    {ok, Messages} ->
      {ok, LineComments} = egithub:pull_req_comments(Cred, Repo, PR),
      {ok, IssueComments} = egithub:issue_comments(Cred, Repo, PR),
      Comments = LineComments ++ IssueComments,
      write_comments(Cred, Repo, PR, Comments, Messages),
      with_warnings;
    {ok, [], TargetUrl} -> {clean, TargetUrl};
    {ok, Messages, TargetUrl} ->
      {ok, LineComments} = egithub:pull_req_comments(Cred, Repo, PR),
      {ok, IssueComments} = egithub:issue_comments(Cred, Repo, PR),
      Comments = LineComments ++ IssueComments,
      write_comments(Cred, Repo, PR, Comments, Messages),
      {with_warnings, TargetUrl};
    {error, Reason} -> {error, Reason};
    {error, Reason, TargetUrl} -> {error, Reason, TargetUrl}
  end.

do_handle_error_source(Error, Module, Cred, Data) ->
  #{<<"number">> := PR, <<"repository">> := Repository} = Data,
  Repo = binary_to_list(maps:get(<<"full_name">>, Repository)),
  {ok, GithubFiles} = egithub:pull_req_files(Cred, Repo, PR),
  case Module:handle_error(Error, Data, GithubFiles) of
    {ok, [], TargetUrl}        -> {clean, TargetUrl};
    {ok, _Messages, TargetUrl} -> {with_warnings, TargetUrl};
    {error, Reason, TargetUrl} -> {error, Reason, TargetUrl}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helper functions

%% @doc Comment files that failed rules.
-spec write_comments(
    egithub:credentials(), string(), map(), [map()], [message()]) -> ok.
write_comments(Cred, Repo, PR, Comments, Messages) ->
  _ = lager:debug(
    "[Github WH] About to write ~p messages (there are already ~p comments)",
    [length(Messages), length(Comments)]),
  Fun =
    fun (#{text := Text, position := 0}) ->
          write_issue_comment(Cred, Repo, PR, Text, Comments);
        (#{commit_id := CommitId,
           path      := Path,
           position  := Position,
           text      := Text
          }) ->
          write_line_comment(Cred, Repo, PR, CommitId,
                             Path, Position, Text, Comments)
    end,
  lists:foreach(Fun, Messages).

write_issue_comment(Cred, Repo, PR, Text, Comments) ->
  case issue_comment_exists(Comments, Text) of
    exists ->
      _ = lager:info("Comment '~s' for issue ~p is already there", [Text, PR]);
    not_exists ->
      egithub:issue_comment(Cred, Repo, PR, Text, #{post_method => queue})
  end.

write_line_comment(Cred, Repo, PR, CommitId, Path, Position, Text, Comments) ->
  case line_comment_exists(Comments, Path, Position, Text) of
    exists ->
      Args = [Text, Path, Position],
      _ = lager:info("Comment '~s' for '~s' on position ~p is already there",
                     Args);
    not_exists ->
      egithub:pull_req_comment_line(
        Cred, Repo, PR, CommitId, Path, Position, Text,
        #{post_method => queue}
       )
  end.

issue_comment_exists(Comments, Body) ->
  MatchingComments =
    [Comment
     || #{<<"issue_url">> := _,
          <<"body">>      := CBody} = Comment <- Comments,
        CBody == Body],

  case MatchingComments of
    [] -> not_exists;
    [_|_] -> exists
  end.

line_comment_exists(Comments, Path, Position, Body) ->
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
