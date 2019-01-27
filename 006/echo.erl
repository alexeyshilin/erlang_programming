-module(echo).

-export([test/0, start/0, stop/0, print/1]).
-export([srv_run/1]).

% 6-1 % ping-pong

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
			exit("Stop signal received"),
			true
	end.


% /Server


start() ->
	Pid = spawn(echo, srv_run, ["Test"]),
	register(srv, Pid),
	true = link(Pid), % ! check true=true !
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
	Pid1 = self(),

	echo:start(),
	echo:print("SrvStart"),
	echo:stop(),

	Pid2 = self(),

	io:format("[%p %p]",[Pid1, Pid2]),

	ok.

% self().
% echo:start().
% echo:print(Term).
% echo:stop().
% self().

% self().
% echo:start().
% echo:print("Z").
% echo:stop().
% self().

% echo:test().