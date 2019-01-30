-module(my_db).
-export([test/0]).
-export([start/0, stop/0, write/2, delete/1, read/1, match/1]).
-export([server_start/1]).

% 7-9

% debug
-define(DEBUG, ok).

-ifndef(DEBUG).
	-define(DEBUG_LOG(Format), ok).
	-define(DEBUG_LOG(Format, Values), ok).
-else.
	-define(DEBUG_LOG(Format), FormatNew = ["[log: "]++Format++["]","\n"], io:format(FormatNew)).
	-define(DEBUG_LOG(Format, Values), FormatNew = ["[log: "]++Format++["]","\n"], io:format(FormatNew, Values)).
-endif.
% /debug

% Db

new() ->
	[].

destroy(_) ->
	ok.

write(Key, Element, Db) ->
	Row = {Key, Element},
	Db ++ [Row].
	% [Db | row].

delete(Key, []) ->
	ok; % is it correct

delete(Key, Db) ->
	delete(Key, Db, []).

read(Key, []) ->
	{error, instance};

read(Key, [{Key2, Element2}|Tdb]) ->
	case {Key2, Element2} of
		{Key, _}	-> {ok, Element2};
		Other		-> read(Key, Tdb)
	end.

match(Element, Db) ->
	match(Element, Db, []).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

delete(Key, [], NewDb) ->
	NewDb;

delete(Key, [Hdb|Tdb], NewDb) ->
	case Hdb of
		[]			-> [];
		{Key, _}	-> delete(Key, Tdb, NewDb);
		Other		-> delete(Key, Tdb, NewDb++[Hdb]) % [Hdb] ++ Hdb
	end.

match(Element, [], []) ->
	[];

match(Element, [], Accum) ->
	Accum;


match(Element, [{Key, Elm}|Tdb], []) ->
	case {Key, Elm} of
		{_, Element}	-> match(Element, Tdb, [Key]);
		Other			-> match(Element, Tdb, [])
	end;

match(Element, [{Key, Elm}|Tdb], Accum) ->
	case {Key, Elm} of
		{_, Element}	-> match(Element, Tdb, Accum++[Key]);
		Other			-> match(Element, Tdb, Accum)
	end.

% /Db

% Server
server_start(Test)->
	server_loop(new()),
	ok.

server_loop(Db)->
	receive
		{From, {write, Key, Element}} ->
			%io:format("~w~n",["write"]),
			?DEBUG_LOG("write ~p ~w", [Key, Element]),
			DbNew = write(Key, Element, Db),
			From ! {self(), response_ok},
			server_loop(DbNew);
		{From, {delete, Key}} ->
			%io:format("~w~n",["delete"]),
			?DEBUG_LOG("delete"),
			DbNew = delete(Key, Db),
			From ! {self(), response_ok},
			server_loop(DbNew);
		{From, {read, Key}} ->
			%io:format("~w~n",["read"]),
			?DEBUG_LOG("read"),
			Res = read(Key,Db),
			From ! {self(), response_ok, Res},
			server_loop(Db);
		{From, {match, Element}} ->
			%io:format("~w~n",["match"]),
			?DEBUG_LOG("match"),
			Res = match(Element, Db),
			From ! {self(), response_ok, Res},
			server_loop(Db);
		{From, Msg} ->
			?DEBUG_LOG("~w~n",[Msg]),
			From ! {self(), response_ok},
			server_loop(Db);
		stop ->
			true
	end.

% /Server

% Client
send_comman(Command)->
	my_db ! {self(), Command},

	receive
		{From, Msg} ->
			io:format("~w~n",[Msg]),
			Msg;
		{From, Msg, Data} ->
			io:format("~w~n",[Msg]),
			Data
	end.
	
	%Msg = ok,
	%Msg.
	

start()->
	Pid = spawn_link(my_db, server_start, ["test"]),
	register(my_db, Pid),
	{ok, Pid}.

stop()->
	my_db ! stop,
	ok.

write(Key, Element)->
	Res = send_comman({write, Key, Element}),
	Res.

delete(Key)->
	Res = send_comman({delete, Key}),
	Res.

read(Key)->
	%{ok, Element} | {error, instance}.
	Res = send_comman({read, Key}),
	Res.

match(Element)->
	%[Keyl, ... , KeyN].
	Res = send_comman({match, Element}),
	Res.

% /Client

% Tests

test()->
	ok.

% /Tests

% c(my_db).
%
% my_db:start().
% my_db:write(100, "Test100").
% my_db:write(200, "Test200").
% my_db:delete(100).
% my_db:stop().
