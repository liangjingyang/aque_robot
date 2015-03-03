%%===========================================================================
%%     FileName: robot_worker.erl
%%       Author: Liangjingyang
%%        Email: simple.continue@gmail.com
%%      Version: 0.1
%%===========================================================================

-module(robot_worker).

-behaviour(gen_server).

-export([
	init/1, 
	handle_call/3, 
	handle_cast/2, 
	handle_info/2, 
	terminate/2, 
	code_change/3
    ]).

-export([
        start_link/1
    ]).

-compile(export_all).

-define(MONITOR, robot_monitor).

-define(TCP_OPTS, [
        binary,
        {packet, 4},
        {reuseaddr, true},
        {nodelay, false},
        {delay_send, true},
        {active, false},
        {exit_on_close, false}
    ]).

-record(state, 
    {
	robot_id = 0,
        id = 0,
        accname,
        socket,
        config,
	cb_mod,
	interval = 1000,
	timer_list = []
    }).

start_link(Id) ->
    gen_server:start_link(?MODULE, [Id], []).

init([Id]) ->
    Ip = robot:get_env(server_ip),
    Port = robot:get_env(server_port),
    CBMod = robot:get_env(cb_mod),
    case gen_tcp:connect(Ip, Port, ?TCP_OPTS) of
        {ok, Socket} ->
            register(robot:get_robot_name(Id), self()),
            ?MONITOR ! {ready_to_work, Id},
            erlang:send_after(1000, self(), {start}),
            random:seed(now()),
            {ok, #state{robot_id = Id, socket = Socket, cb_mod = CBMod}};
        Error ->
            {stop, Error}
    end.

handle_call({show_state}, _From, State) ->
    {reply, State, State};

handle_call({show_dic, Key}, _From, State) ->
    {reply, get_dic(Key), State};

handle_call(_Request, _From, State) ->
    robot:log("robot_worker, unknow === call === : request=~w, From=~w", [_Request, _From]),
    {noreply, State}.

handle_cast(_Request, State) ->
    robot:log("robot_worker, unknow === cast === : request=~w", [_Request]),
    {noreply, State}.

handle_info({inet_async, Socket, _Ref, {ok, Proto}}, State) ->
    Data = robot_proto:decode_data(Proto),
    State2 = response(Data, State),
    async_recv(Socket, 0, -1),
    {noreply, State2};

handle_info({inet_async, Socket, _, {error,timeout}}, State) ->
    async_recv(Socket, 0, -1),
    {noreply, State};

handle_info({inet_async, _Socket, _, {error,closed}}, State) ->
    robot:log("robot_worker, tcp_recv_closed, id=~w", [State#state.robot_id]),
    {stop, normal, State};

handle_info({start}, #state{cb_mod = CBMod, socket = Socket} = State) ->
    State2 = CBMod:start(State),
    async_recv(Socket, 0, -1),
    {noreply, State2};

handle_info({do_timer, Name}, State) ->
    State2 = do_timer(Name, State),
    {noreply, State2};

handle_info({func, Func, Args}, State) ->
    Res = apply(Func, Args),
    robot:log("robot_worker, func result : ~w", [Res]),
    {noreply, State};

handle_info({stop, Reason}, State) ->
    {stop, Reason, State};

handle_info(_Msg, State) ->
    robot:log("robot_worker, unknow === info === : msg=~w", [_Msg]),
    {noreply, State}.
   
terminate(Reason, State) ->
    robot_monitor ! {robot_worker_dead, State#state.robot_id, Reason},
    ok.

code_change(_, State, _) ->
    {ok, State}.

async_recv(Sock, Length, Timeout) when is_port(Sock) ->
    case prim_inet:async_recv(Sock, Length, Timeout) of
        {ok, Res} -> 
	    Res; 
        Res -> 
	    erlang:throw(Res)
    end.

%% ====================================================================================

request(Data, #state{cb_mod = CBMod, socket = Socket}) ->
    Proto = CBMod:encode(Data),
    gen_tcp:send(Socket, Proto).

response(Proto, #state{cb_mod = CBMod} = State) ->
    Data = CBMod:decode(Proto),
    State2 = CBMod:response(Data, State),
    State2.

start_timer(Name, Interval, State) ->
    Timer = {Name, Interval},
    TimerList = lists:keystore(Name, 1, State#state.timer_list, Timer),
    erlang:send_after(Interval, self(), {do_timer, Name}),
    State#state{timer_list = TimerList}.

stop_timer(Name, State) ->
    TimerList = lists:keydelete(Name, 1, State#state.timer_list),
    State#state{timer_list = TimerList}.
    
do_timer(Name, State) ->
    case lists:keyfind(Name, 1, State#state.timer_list) of
	{Name, Interval} ->
	    erlang:send_after(Interval, self(), {do_timer, Name}),
	    case Name of
		random_proto ->
		    State2 = random_proto(State);
		_ ->
		    State2 = (State#state.cb_mod):timer(Name)
	    end;
	false ->
	    State2 = State 
    end,
    State2.

start_random_proto(Interval, State) ->
    start_timer(random_proto, Interval, State).

stop_random_proto(State) ->
    stop_timer(random_proto, State).


%% 行为
random_proto(#state{cb_mod = CBMod} = State) ->
    ProtoList = CBMod:proto_list(),
    [{_, Action, Args}] = robot:random_by_rate(ProtoList, 1),
    State2 = ?MODULE:Action(Args, State),
    State2.


set_dic(Key, Value) ->
    erlang:put(Key, Value).

get_dic(Key) ->
    erlang:get(Key).

erase_dic(Key) ->
    erlang:erase(Key).

