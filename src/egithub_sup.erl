-module(egithub_sup).

-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link() -> supervisor:startlink_ret().
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

-spec init(term()) -> term().
init({}) ->
  Table = ets:new(?MODULE, [set, named_table, public]),
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
