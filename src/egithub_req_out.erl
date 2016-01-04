%% @hidden
-module(egithub_req_out).

-behavior(gen_server).

-define(DELAY_MILLIS, [500, 1000, 2000, 4000, 8000, 16000, 32000, 64000]).

-export([ start_link/1
        , init/1
        , terminate/2
        , code_change/3
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        ]).

-type state() :: #{ table  => ets:tab()
                  , delays => [pos_integer()]
                  }.

-spec start_link(ets:tab()) -> {ok, pid()}.
start_link(Table) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Table, []).

%% @private
-spec init(ets:tab()) -> {ok, state(), pos_integer()}.
init(Table) ->
  State =
    #{ table => Table
     , delays => ?DELAY_MILLIS
     },
  {ok, State, hd(?DELAY_MILLIS)}.

%% @private
-spec handle_info(_, state()) -> {noreply, state(), pos_integer()}.
handle_info(timeout, State) ->
  #{table := Table} = State,
  #{delays := [NextDelay|_]} = NewState =
    case ets:first(Table) of
      '$end_of_table' ->
        State#{delays => ?DELAY_MILLIS};
      Key ->
        [Request] = ets:lookup(Table, Key),
        case {process_request(Request), State} of
          {ok, State} ->
            true = ets:delete(Table, Key),
            State#{delays => ?DELAY_MILLIS};
          {retry, #{delays := [_Delay]}} ->
            State;
          {retry, #{delays := [_Delay|Delays]}} ->
            State#{delays => Delays};
          {error, State} ->
            true = ets:delete(Table, Key),
            State#{delays => ?DELAY_MILLIS}
        end
    end,
  _ =
    case ets:info(Table, size) of
      0 -> ok;
      Size -> lager:info("[Github API] ~p queued requests", [Size])
    end,
  {noreply, NewState, NextDelay};
handle_info(_Info, State) ->
  {noreply, State, ?DELAY_MILLIS}.

%% @private
-spec handle_call(Msg, _From, state()) ->
  {stop, {unknown_request, Msg}, {unknown_request, Msg}, state()}.
handle_call(Msg, _From, State) ->
  {stop, {unknown_request, Msg}, {unknown_request, Msg}, State}.

%% @private
-spec handle_cast(Msg, state()) -> {stop, {unknown_request, Msg}, state()}.
handle_cast(Msg, State) -> {stop, {unknown_request, Msg}, State}.

%% @private
-spec terminate(_, state()) -> ok.
terminate(_Reason, _State) -> ok.

%% @private
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

-spec process_request(egithub_req:req()) -> ok | retry | error.
process_request(Request) ->
  case egithub_req:run(Request) of
    {ok, _RespBody} -> ok;
    {error, {"403", RespHeaders, RespBody}} ->
      case lists:keyfind("X-RateLimit-Limit", 1, RespHeaders) of
        false ->
          _ = lager:warning(
            "[Github API] Error:~nRequest: ~p~nError: ~p~n",
            [Request, {"403", RespHeaders, RespBody}]),
          error;
        _ ->
          _ = lager:warning("[Github API] Rate limited: ~p", [RespHeaders]),
          retry
      end;
    {error, {Status, RespHeaders, RespBody}} ->
      _ = lager:warning(
        "[Github API] Error:~nRequest: ~p~nError: ~p~n",
        [Request, {Status, RespHeaders, RespBody}]),
      error
  end.
