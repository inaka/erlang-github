-module(egithub_sup).

-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link() -> supervisor:startlink_ret().
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

-spec init({}) -> {ok, {{supervisor:strategy(),
                         non_neg_integer(),
                         non_neg_integer()},
                        [supervisor:child_spec()]}}.
init({}) ->
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
