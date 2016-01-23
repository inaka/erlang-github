%% @hidden
-module(egithub_sup).

-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link() -> {'ok', pid()} | {'error', term()}.
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, noargs).

-spec init(noargs) ->
  {ok, {{one_for_one, 5, 10}, [supervisor:child_spec()]}}.
init(noargs) ->
  Table = egithub_req:create_table(),
  {ok,
    { {one_for_one, 5, 10}
    , [ { egithub_req_in
        , {egithub_req_in, start_link, [Table]}
        , permanent, 1000, worker, [egithub_req_in]
        }
      , { egithub_req_out
        , {egithub_req_out, start_link, [Table]}
        , permanent, 1000, worker, [egithub_req_in]
        }
      ]
    }}.
