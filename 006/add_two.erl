-module(add_two).
-export([start/0, request/1, loop/0]).

start() ->
	process_flag(trap_exit, true),
	Pid = spawn_link(add_two, loop, []),
	register(add_two, Pid),
	{ok, Pid}.

request(Int) ->
	add_two ! {request, self(), Int},
	receive
		{result, Result}
			-> Result;
		{'EXIT', _Pid, Reason}
			-> {error, Reason}
		after 1000
			-> timeout
	end.

loop() ->
	receive
		{request, Pid, Msg} ->
			Pid ! {result, Msg + 2}
	end,

loop().


% 1> c(add_two).
% {ok,add_two}
% 2> add_two:start().
% {ok,<0.119.0>}
% 3> add_two:request(6).
% 8
% 4> add_two:request(six).
