%%===========================================================================
%%     FileName: robot.erl
%%       Author: Liangjingyang
%%        Email: simple.continue@gmail.com
%%      Version: 0.1
%%===========================================================================

-module(robot).

-export([
        start/0,
        start/2,
        stop/0,
        stop/1,
        log/2,
        add_robot/1
    ]).

-export([
	get_env/1,
	get_env/2,
        get_all_robot/0,
        get_all_robot_num/0,
        get_robot_name/1,
        get_robot_state/1,
        get_robot_dic/2,
        get_proc_info/1,
        call_robot/2,
        info_robot/2,
        random_from_list/1,
	random_by_rate/2,
	now/0,
        unixtime/0
    ]).

-define(APP, aque_robot).

start() ->
    application:start(?APP).

stop() ->
    application:stop(?APP).

start(normal, []) ->
    robot_sup:start_link().

stop(_) ->
    ok.

add_robot(Num) ->
    case whereis(robot_monitor) of
        undefined -> 
            robot:log("I'm sorry, the robot monitor dead!", []);
        Pid ->
            Pid ! {add_worker, Num}
    end.

get_env(Key) ->
    get_env(Key, undefined).

get_env(Key, Default) ->
    case application:get_env(?APP, Key) of
	undefined ->
	    Default;
	Value ->
	    Value
    end.

    

get_all_robot() ->
    F = fun("irobot_" ++ _ = Name) ->
            {Name, whereis(list_to_atom(Name))};
        (_) ->
            undefined
    end,
    [Res || Name <- registered(), undefined =/= (Res = F(atom_to_list(Name)))].

get_proc_info(Id) ->
    case whereis(get_robot_name(Id)) of
        undefined -> 
            io:format("I'm sorry, the robot dead!~n", []);
        Pid ->
            io:format("Proc_info Id=~w: ~p", [Id, process_info(Pid)])
    end.


get_all_robot_num() ->
    length(get_all_robot()).

get_robot_name(Id) ->
    list_to_atom(lists:concat(["irobot_", Id])).

log(Format, Args) ->
    case whereis(robot_monitor) of
	undefined ->
	    io:format(Format ++ "\n", Args);
	Pid ->
	    Pid ! {log, {Format, Args}}
    end.

%log(_Format, _Args) ->
    %ignore.

now() ->
    unixtime().

unixtime() ->
    {MegaSecs, Secs, _MicroSecs} = erlang:now(),
     MegaSecs * 1000000 + Secs.

get_robot_state(Id) ->
    call_robot(Id, {show_state}).

get_robot_dic(Id, Key) ->
    call_robot(Id, {show_dic, Key}).

call_robot(Id, Msg) ->
    case whereis(get_robot_name(Id)) of
        undefined -> 
            io:format("I'm sorry, the robot dead!~n", []);
        Pid ->
            Reply = gen_server:call(Pid, Msg),
            io:format("Reply of ~w : ~p~n", [Msg, Reply])
    end.
info_robot(Id, Msg) ->
    case whereis(get_robot_name(Id)) of
        undefined -> 
            io:format("I'm sorry, the robot dead!~n", []);
        Pid ->
            Pid ! Msg
    end.

random_from_list(List) ->
    N = random:uniform(length(List)),
    lists:nth(N, List).


random_by_rate(List, N) ->
    RateList = [erlang:element(N, T)||T <- List],
    case in_rate(RateList) of
	{X, _} ->
	    [lists:nth(X, List)];
	L when is_list(L) andalso length(L) > 0 ->
	    [lists:nth(X, List)||{X, _} <- L];
	_ ->
	    []
    end.


in_rate(List) ->
    {_, Total, List2} = 
    lists:foldl(fun(R, {N, Add, L}) ->
			{N+1, Add+R, [{N, R, Add, Add + R}|L]}
		end, {1, 0, []}, List),
    Random = random:uniform(Total),
    List3 = lists:reverse(List2),
    in_rate2(List3, Random, List3).

in_rate2([], _, _) ->
    {0, 0};
in_rate2([{N, R, Min, Max} = This|L], Random, List) ->
    if 
	Random > Min andalso Random =< Max ->
	    {N, R};
	Random > Max ->
	    in_rate2(L, Random, List);
	true ->
	    robot:log("in_rate2 error, random: ~w, List: ~w, This: ~w", [Random, List, This])
    end.
