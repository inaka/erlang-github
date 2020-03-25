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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type message() :: #{commit_id => string(),
                     path      => binary(),
                     position  => pos_integer(),
                     text      => binary()
                    }.
-type review_comment() :: #{path      => string() | binary(),
                            position  => pos_integer(),
                            body      => string() | binary()
                           }.
-type review_event() :: string() | binary(). % "APPROVE" | "REQUEST_CHANGES".
-type review() :: #{commit_id => string() | binary(),
                    body      => string() | binary(),
                    event     => review_event(),
                    comments  => [review_comment()]
                   }.
-type file() :: map().
-type req_data() :: map().
-type webhook_target_url() :: string()|undefined.
-export_type([req_data/0, message/0, file/0, webhook_target_url/0, review/0]).

-callback handle_pull_request(egithub:credentials(), req_data(), [file()]) ->
  {ok, [message()]} | {ok, review()} | {error, term()}.

-callback handle_error({error, term()}, req_data(), [file()]) ->
    {error, term(),  webhook_target_url()}
  | {ok,    [map()], webhook_target_url()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Should be called from the endpoint that handles the GitHub's request
%%      for the webhook.
%% @end
-spec event(atom(), egithub:credentials(), egithub_webhook_req:request()) ->
      ok | {error, term()}.
event(Module, Cred, Request) ->
  case egithub_webhook_req:header(<<"x-github-event">>, Request) of
    undefined ->
      {error, missing_header};
    <<"ping">> -> ok;
    <<"pull_request">> ->
      {_Request2, EventData} = egithub_webhook_req:payload(Request),
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
  egithub_webhook_req:request()) -> ok | {error, term()}.
event(Module, StatusCred, ToolName, Context, CommentsCred, Request) ->
  case egithub_webhook_req:header(<<"x-github-event">>, Request) of
    undefined ->
      {error, missing_header};
    <<"ping">> -> ok;
    <<"pull_request">> ->
      {_Request2, EventData} = egithub_webhook_req:payload(Request),
      set_status(pending, StatusCred, ToolName, Context, EventData),
      try
        Result = do_handle_pull_request(Module, CommentsCred, EventData),
        specify_status(Result, StatusCred, ToolName, Context, EventData)
      catch
        _:Error ->
          _ = lager:warning(
            "event:Error ~p Module: ~p ToolName: ~p "
            "Context: ~p EventData: ~p",
            [ Error
            , Module
            , ToolName
            , Context
            , EventData]),
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
                    , jsx:json_term()) -> ok | {error, any()}.
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

do_handle_pull_request(Module, Cred, #{<<"number">> := PR,
                                       <<"repository">> := RepoInfo} = Data) ->
  Repo = binary_to_list(maps:get(<<"full_name">>, RepoInfo)),
  {ok, GithubFiles} = egithub:pull_req_files(Cred, Repo, PR),
  case Module:handle_pull_request(Cred, Data, GithubFiles) of
    {ok, []} -> clean;
    {ok, #{event := <<"APPROVE">>, comments := []} = Messages} ->
      ok = handle_messages_or_review(Cred, Repo, PR, Messages),
      clean;
    {ok, Messages} ->
      ok = handle_messages_or_review(Cred, Repo, PR, Messages),
      with_warnings;
    {ok, [], TargetUrl} -> {clean, TargetUrl};
    {ok, Messages, TargetUrl} ->
      ok = handle_messages_or_review(Cred, Repo, PR, Messages),
      {with_warnings, TargetUrl};
    {error, Reason} -> {error, Reason};
    {error, Reason, TargetUrl} -> {error, Reason, TargetUrl}
  end.

-spec do_handle_error_source(term(), atom(), egithub:credentials(), map()) ->
     {clean, webhook_target_url()}
   | {with_warnings, webhook_target_url()}
   | {error, term(), webhook_target_url()}.
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
    egithub:credentials(), string(), integer(), [map()], [message()]) -> ok.
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

-spec handle_messages_or_review(egithub:credentials(), egithub:repository(),
                                integer(), review() | [message()]) ->
  egithub:result().
handle_messages_or_review(Cred, Repo, PR, Messages) ->
  case application:get_env(egithub, review_style, pr_review) of
    individual_comments ->
      {ok, LineComments} = egithub:pull_req_comments(Cred, Repo, PR),
      {ok, IssueComments} = egithub:issue_comments(Cred, Repo, PR),
      Comments = LineComments ++ IssueComments,
      write_comments(Cred, Repo, PR, Comments, Messages);
    pr_review ->
      dismiss_old_pr_reviews(Cred, Repo, PR),
      % Without this sleep call, the dismissal message will appear below the
      % new review, so with this sleep call, the dismissal message will appear
      % above the new review.
      timer:sleep(500),
      write_pr_review(Cred, Repo, PR, Messages)
  end.

-spec write_pr_review(egithub:credentials(), egithub:repository(), integer(),
                      review())->
  egithub:result().
write_pr_review(Cred, Repo, PR, PrReview) ->
  egithub:pr_review(Cred, Repo, PR, PrReview, #{post_method => queue}).

-spec dismiss_old_pr_reviews(egithub:credentials(), egithub:repository(),
                            integer()) ->
  ok.
dismiss_old_pr_reviews(Cred, Repo, PR) ->
  {ok, ExistingReviews} = egithub:pr_reviews(Cred, Repo, PR),
  Fun =
    fun(#{<<"id">> := RId,
          <<"user">> := #{<<"login">> := <<"elvisci">>},
          <<"state">> := State}) ->
         case State of
           <<"DISMISSED">> ->
             % Don't try to dismiss an already dismissed review ;)
             ok;
           _ ->
             Body = #{message => <<"Old/outdated review">>},
             {ok, _} = egithub:dismiss_pr_review(Cred, Repo, PR, RId, Body)
         end;
       (_) ->
         ok
    end,
  _ = lists:foreach(Fun, ExistingReviews),
  ok.
