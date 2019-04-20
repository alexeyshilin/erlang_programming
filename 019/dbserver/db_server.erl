-module(db_server).
-export([start/0 , stop/0, upgrade/1]).
-export([write/2 , read/1, delete/1]).
-export([init/0, loop/1]).

-vsn(1.0).

start() ->
	register(db_server, spawn(db_server, init, [])).

stop()->
	db_server ! stop.

upgrade(Data) ->
	db_server ! {upgrade, Data}.

write(Key, Data) ->
	db_server ! { write, Key, Data}.

read(Key) ->
	db_server ! {read, self(), Key},
	receive Reply -> Reply end.

delete(Key) ->
	db_server ! {delete, Key}.

init() ->
	loop(db:new()).

loop(Db) ->
	receive
		{write, Key, Data} ->
			loop(db:write(Key, Data, Db));
		{read, Pid, Key} ->
			Pid ! db:read(Key, Db),
			loop(Db);
		{delete, Key} ->
			loop(db:delete(Key, Db));
		{upgrade, Data} ->
			NewDb = db:convert(Data, Db),
			db_server:loop(NewDb);
		stop ->
			db:destroy(Db)
	end.

% c(db).
% c(db_server).
%
% db_server:start().
% db_server:write(francesco, sanfrancisco).
% db_server:write(alison, london).



% cd("/var/projects/test/erl/019/dbserver/patches/001").
% compile:file(db).

% db:module_info().

% code:root_dir().
% code:add_patha("/var/projects/test/erl/019/dbserver/patches/001").
% code:get_path().
% code:load_file(db).
% code:is_loaded(db).
% code:soft_purge(db).
% db:module_info(attributes).
%
% db_server:read(alison).
% db_server:read(martin).




% cd("/var/projects/test/erl/019/dbserver/patches/002").
% compile:file(db).

% code:add_patha("/var/projects/test/erl/019/dbserver/patches/002").
% code:get_path().
% code:load_file(db).
% code:is_loaded(db).
% db:module_info().
% code:soft_purge(db).
% db:module_info(attributes).
% db_server:upgrade(dict).
% db:module_info().
% db_server:write(martin, cairo).
%
% db_server:read(francesco).
% db_server:read(martin).
