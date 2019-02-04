-module(my_db).
-export([test/0]).
-export([start/0, stop/0, update/0, write/2, delete/1, read/1]).
-export([server_start/1]).


% Server
server_start(Test)->
	server_loop(db:new()),
	ok.

server_loop(Db)->
	receive
		{From, {write, Key, Element}} ->
			io:format("[write ~p ~w]", [Key, Element]),
			DbNew = db:write(Key, Element, Db),
			From ! {self(), response_ok},
			server_loop(DbNew);
		{From, {delete, Key}} ->
			io:format("[delete]"),
			DbNew = db:delete(Key, Db),
			From ! {self(), response_ok},
			server_loop(DbNew);
		{From, {read, Key}} ->
			io:format("[read]"),
			Res = db:read(Key,Db),
			From ! {self(), response_ok, Res},
			server_loop(Db);
		{From, {upgrade, null}} ->
			io:format("[upgrade]"),
			code:add_patha("/var/projects/test/erl/008/patches"), %% path!!!
			compile:file(db),
			code:load_file(db),
			code:soft_purge(db),
			NewDb = db:code_upgrade(Db),
			From ! {self(), response_ok},
			server_loop(NewDb);
		{From, Msg} ->
			io:format("~w~n",[Msg]),
			From ! {self(), response_ok},
			server_loop(Db);
		stop ->
			io:format("[stop: ~p]", [Db]),
			true
	end.

% /Server

% Client
send_comman(Command)->
	db ! {self(), Command},

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
	Pid = spawn(my_db, server_start, ["test"]),
	register(db, Pid),
	ok.

stop()->
	db ! stop,
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

update()->
	Res = send_comman({upgrade, null}),
	Res.

% /Client

% Tests

test()->
	ok.

% /Tests

% cd("/var/projects/test/erl/003").
% compile:file(db).
% code:load_file(db).
%
% cd("/var/projects/test/erl/008").
% compile:file(my_db).
% code:load_file(my_db).
%
% my_db:start().
% my_db:write(100, "Test100").
% my_db:write(200, "Test200").
%
% my_db:update().
% my_db:write(300, "Test300").
%
% my_db:stop().
