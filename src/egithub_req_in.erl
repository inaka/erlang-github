%% @hidden
-module(egithub_req_in).

-behavior(gen_server).

-export([ start_link/1
        , init/1
        , terminate/2
        , code_change/3
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        ]).

-type state() :: #{table => ets:tab_name()}.

-spec start_link(ets:tab_name()) -> {ok, pid()}.
start_link(Table) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Table, []).

%% @private
-spec init(ets:tab_name()) -> {ok, state()}.
init(Table) -> {ok, #{table => Table}}.

%% @private
-spec handle_call(Msg, _From, state()) ->
  {stop, {unknown_request, Msg}, {unknown_request, Msg}, state()}.
handle_call(Msg, _From, State) ->
  {stop, {unknown_request, Msg}, {unknown_request, Msg}, State}.

%% @private
-spec handle_cast(egithub_req:req(), state()) -> {noreply, state()}.
handle_cast(Request, State) ->
  #{table := Table} = State,
  true = ets:insert(Table, Request),
  lager:info("[Github API] ~p queued requests", [ets:info(Table, size)]),
  {noreply, State}.

%% @private
-spec handle_info(_, state()) -> {noreply, state()}.
handle_info(_Info, State) -> {noreply, State}.

%% @private
-spec terminate(_, state()) -> ok.
terminate(_Reason, _State) -> ok.

%% @private
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
