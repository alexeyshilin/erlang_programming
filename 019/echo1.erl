-module(echo1).
-export([go/0, loop/0]).
-include_lib("eunit/include/eunit.hrl").

go() ->
	Pid = spawn(echo1, loop, []),
	Pid ! {self(), hello},

	receive
		{Pid, Msg} ->
			io:format("~w~n",[Msg])
	end,
	Pid ! stop.

loop() ->
	receive
		{From, Msg} ->
			From ! {self(), Msg},
			loop();
		stop ->
			true
	end.

% c(echo1).
%
% echo1:go().

setup1_test_() ->
	{spawn,
		{setup,
		fun () -> ok end, 			% init
		fun (_) -> ok end,				% clear
		?_assertMatch(stop, echo1:go())		% test
		}
	}.

% c(echo1).
%
% echo1:test().
% eunit:test(echo1).
