%%===========================================================================
%%     FileName: robot_worker_sup.erl
%%       Author: Liangjingyang
%%        Email: simple.continue@gmail.com
%%      Version: 0.1
%%===========================================================================

-module(robot_worker_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 0, 10},
          [{robot_worker, {robot_worker, start_link, []},
            transient, brutal_kill, worker, [robot_worker]}]}}.
