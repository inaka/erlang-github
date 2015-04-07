-module(egithub_webhook).

-export([event/3, event/6]).

-export_type([request/0]).

-type event() :: pull_request.
-type request() :: #{headers => map(), body => map()}.

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

-spec event(
  atom(), egithub:credentials(), string(), string(), egithub:credentials(),
  request()) -> ok | {error, term()}.
event(Module, StatusCred, ToolName, Context, CommentsCred, Request) ->
  #{headers := Headers, body := Body} = Request,
  case maps:get(<<"x-github-event">>, Headers, undefined) of
    undefined ->
      {error, missing_header};
    EventName ->
      EventData = jiffy:decode(Body, [return_maps]),
      set_status(pending, StatusCred, ToolName, Context, EventData),
      try event(Module, CommentsCred, EventName, EventData) of
        ok ->
          set_status(success, StatusCred, ToolName, Context, EventData),
          ok;
        {error, Error} ->
          set_status({error, Error}, StatusCred, ToolName, Context, EventData),
          {error, Error}
      catch
        _:Error ->
          set_status(
            {failure, Error}, StatusCred, ToolName, Context, EventData),
          throw(Error)
      end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Statuses
set_status(FullState, StatusCred, ToolName, Context, EventData) ->
  #{ <<"repository">> := Repository
   , <<"pull_request">> := #{<<"head">> := Head}
   } = EventData,
  Repo = binary_to_list(maps:get(<<"full_name">>, Repository)),
  Sha = binary_to_list(maps:get(<<"sha">>, Head)),
  Description = status_description(FullState, ToolName),
  State = normalize_state(FullState),
  egithub:create_status(StatusCred, Repo, Sha, State, Description, Context).

status_description(pending, ToolName) ->
  ToolName ++ " is checking your pull request";
status_description(success, ToolName) ->
  ToolName ++ " is satisfied with your pull request";
status_description({error, Error}, ToolName) ->
  IoData =
    io_lib:format("~s is not happy with your PR: `~p`", [ToolName, Error]),
  binary_to_list(iolist_to_binary(IoData));
status_description({failure, Error}, ToolName) ->
  IoData =
    io_lib:format("~s failed while reviewing your PR: `~p`", [ToolName, Error]),
  binary_to_list(iolist_to_binary(IoData)).

normalize_state({State, _}) -> State;
normalize_state(State) -> State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Events

-spec event(atom(), egithub:credentials(), event(), map()) ->
        ok | {error, term()}.
event(Module, Cred, <<"pull_request">>,
      #{<<"number">> := PR, <<"repository">> := Repository} = Data) ->
  Repo = binary_to_list(maps:get(<<"full_name">>, Repository)),
  {ok, GithubFiles} = egithub:pull_req_files(Cred, Repo, PR),
  case Module:handle_pull_request(Cred, Data, GithubFiles) of
    {ok, Messages} ->
          {ok, LineComments} = egithub:pull_req_comments(Cred, Repo, PR),
          {ok, IssueComments} = egithub:issue_comments(Cred, Repo, PR),
          Comments = LineComments ++ IssueComments,
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
    lager:debug(
      "[Github WH] About to write ~p messages (there are already ~p comments)",
      [length(Messages), length(Comments)]),
    Fun =
        fun
            (#{text := Text,
               position := 0
              }) ->
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
            Args = [Text, PR],
            lager:info("Comment '~s' for issue ~p is already there", Args);
        not_exists ->
            egithub:issue_comment(
                Cred, Repo, PR, Text, #{post_method => queue})
    end.

write_line_comment(Cred, Repo, PR, CommitId, Path, Position, Text, Comments) ->
    case line_comment_exists(Comments, Path, Position, Text) of
        exists ->
            Args = [Text, Path, Position],
            lager:info("Comment '~s' for '~s' on position ~p is already there",
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
