-module(echo2).
-export([go/0, loop/0]).
-include_lib("eunit/include/eunit.hrl").

go() ->
	register(echo, spawn(echo2, loop, [])),
	echo ! {self(), hello},

	receive
		{_Pid, Msg} ->
			io:format("~w~n",[Msg])
	end,
	%echo ! stop,
	ok.

loop() ->
	receive
		{From, Msg} ->
			From ! {self(), Msg},
			loop();
		stop ->
			true
	end.



% c(echo2).
%
% echo2:go().
% whereis(echo).
% echo ! stop.
% whereis(echo).
% regs().

setup1_test_() ->
	{spawn,
		{setup,
		fun () -> ok end, 			% init
		fun (_) -> ok end,				% clear

		% test
		[
			?_assertMatch(ok, echo2:go())
			,?_assertMatch(Pid when is_pid(Pid), whereis(echo))
			,?_assertMatch(stop, echo ! stop)
			,?_assertMatch(undefined, whereis(echo))
		]
		}
	}.

% c(echo2).
%
% echo2:test().
% eunit:test(echo2).
