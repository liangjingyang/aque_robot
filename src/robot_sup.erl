%%===========================================================================
%%     FileName: robot_sup.erl
%%       Author: Liangjingyang
%%        Email: simple.continue@gmail.com
%%      Version: 0.1
%%===========================================================================

-module(robot_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

init([]) ->
    {ok,
        {{one_for_all, 10, 10},
            [
                {
                    robot_worker_sup,
                    {robot_worker_sup, start_link, []},
                    transient,
                    infinity,
                    supervisor,
                    [robot_worker_sup]
                },
                {
                    robot_monitor,
                    {robot_monitor, start_link, []},
                    transient,
                    100,
                    worker,
                    [robot_monitor]
                }
            ]
        }
    }.

