-module(echo).

-export([test/0, start/0, stop/0, print/1]).
-export([srv_run/1]).

% 4-1

% io:format("~w~n",[Msg])


% Server

srv_run(Param)->
	receive
		{From, Msg} ->
			io:format("[test]"),
			io:format("~w~n",[Msg]),
			From ! { self() , Msg},
			%From ! { self() , received},
			srv_run("SelfTest");
		stop -> 
			true
	end.


% /Server


start() ->
	Pid = spawn(echo, srv_run, ["Test"]),
	register(srv, Pid),
	ok.

print(Message) ->
	srv ! {self(), Message},

	receive
		{ Pid, Msg} ->
			io:format("print: ~w~n",[Msg])
	end,

	ok.

stop() ->
	srv ! stop,

%	receive
%		{ Pid, Msg} ->
%			io:format("stop: ~w~n",[Msg])
%	end,

	ok.

test()->
	echo:start(),
	echo:print("SrvStart"),
	echo:stop(),

	ok.

% echo:start().
% echo:print(Term).
% echo:stop().

% echo:start().
% echo:print("Z").
% echo:stop().

% echo:test().