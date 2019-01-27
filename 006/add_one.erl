-module(add_one).
-export([start/0, request/1, loop/0]).

start() ->
	%register(add_one, spawn_link(add_one, loop, [])).
	process_flag(trap_exit, true),
	Pid = spawn_link(add_one, loop, []),
	register(add_one, Pid),
	{ok, Pid}.

request(Int) ->
	add_one ! {request, self() , Int},
	receive
		{ result, Result} -> Result;
		{'EXIT', _Pid, Reason} -> {error, Reason}
		after 1000 -> timeout
	end.

loop() ->
	receive
		{request, Pid, Msg} ->
		Pid ! { result, Msg + 1}
	end,
	loop().


% 1> self().
% <0.29.0>
% 2> add_one:start().
% true
% 3> add_one:request(1).
% 2
% 4> add_one:request(one).
