-module(myfact).
-export([test/0]).
-export([myfact/1]).

myfact(ruby) ->
	Cmd = "ruby myfact.rb",
	myfact(Cmd);

myfact(php) ->
	Cmd = "php myfact.php",
	myfact(Cmd);

myfact(Cmd) ->
	%Cmd = "ruby myfact.rb",
	%Cmd = "php myfact.php",
	Port = open_port({spawn, Cmd}, [use_stdio]),
	port_command(Port, "12\r\n"),
	receive
		{Port, {data, Data}} ->
			io:format("got data: ~p~n", [Data]),
			ok
		after 1000 -> timeout
	end.

test() ->
	ok.

% c(myfact).
%
% myfact:myfact(php).
% myfact:myfact(ruby).
