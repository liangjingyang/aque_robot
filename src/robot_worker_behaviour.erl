%%===========================================================================
%%     FileName: robot_worker_behaviour.erl
%%       Author: Liangjingyang
%%        Email: simple.continue@gmail.com
%%      Version: 0.1
%%===========================================================================

-module(robot_worker_behaviour).

-callback start(State::record()) -> State2::record().
-callback encode(Data::any()) -> Data2::any().
-callback deencode(Data::any()) -> Data2::any().
-callback encrypt(Data::any()) -> Data2::any().
-callback unencrypt(Data::any()) -> Data2::any().
-callback request(Data::any(), State::record()) -> Data2::any().
-callback response(Data::any(), State::record()) -> Data2::any().
-callback proto_list() -> ProtoList::list().


