%%===========================================================================
%%     FileName: robot_monitor.erl
%%       Author: Liangjingyang
%%        Email: simple.continue@gmail.com
%%      Version: 0.1
%%===========================================================================
-module(robot_monitor).

-export([
        start_link/0
    ]).

-export([
	init/1, 
	handle_call/3, 
	handle_cast/2, 
	handle_info/2, 
	terminate/2, 
	code_change/3
    ]).

-record(state, 
    {
	begin_id = 1,
        log_file="robot.log"
    }).

-define(APP, aque_robot).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    LogFile = make_log_file(),
    BeginId = robot:get_env(begin_id, 1),
    {ok, #state{begin_id = BeginId, log_file = LogFile}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({ready_to_work, Id}, State) ->
    ready_to_work(State, Id),
    {noreply, State};

handle_info({worker_dead, Id, Reason}, State) ->
    worker_dead(State, Id, Reason),
    {noreply, State};

handle_info({add_worker, Num}, #state{begin_id = BeginId} = State) ->
    add_worker(Num, BeginId),
    {noreply, State#state{begin_id = BeginId + Num}};

handle_info({log, Log}, #state{log_file = File} = State) ->
    do_log_file(File, Log),
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

add_worker(WorkerNum, BeginId) ->
    Fun = fun(N) ->
            timer:sleep(1),
            supervisor:start_child(robot_worker_sup, [N])
    end,
    EndId = BeginId + WorkerNum - 1,
    [Fun(N) || N <- lists:seq(BeginId, EndId)].

ready_to_work(#state{log_file = File}, Id) ->
    Log = {"Okie dokie! id: ~w", [Id]},
    do_log_file(File, Log).

worker_dead(#state{log_file = File}, Id, Reason) ->
    Log = {"For the Lich King! id: ~w, Reason: ~w", [Id, Reason]},
    do_log_file(File, Log).

do_log_file(File, {Format, Args}) ->
    {{Y, M, D}, {H, MM, S}} = calendar:local_time(),
    Time = io_lib:format("==== ~w-~w-~w ~w:~w:~w ==== ", [Y,M,D,H,MM,S]),
    Data = io_lib:format(Format, Args),
    file:write_file(File, Time ++ Data ++ "\n", [append]).

make_log_file() ->
    LogPath = robot:get_env(log_path, ""),
    {Y, M, D} = date(),
    lists:concat([LogPath, "robot_log_", Y, "_", M, "_", D, ".log"]).
