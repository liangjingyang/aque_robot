%% Copyright (c) 2013-2015, Liangjingyang <simple.continue@gmail.com>

{application, aque_robot, [
    {id, "aque_robot"},
    {description, "Game server robot written in Erlang"},
    {vsn, "0.1"},
    {modules, []},
    {registered, [robot_sup, robot_monitor, robot_worker_sup]},
    {applications, [
	kernel,
	stdlib,
	crypto
    ]},
    {mod, {robot, []}},
    {env, [
	{begin_id, 10001},
	{worker_num, 1},
	{server_ip, "10.60.150.66"},
	{server_port, 8880}
    ]}
]}.

