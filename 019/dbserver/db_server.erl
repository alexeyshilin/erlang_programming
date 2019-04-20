-module(db_server).
-export([start/0 , stop/0, upgrade/1]).
-export([write/2 , read/1, delete/1]).
-export([init/0, loop/1]).

-vsn(1.0).

-include_lib("eunit/include/eunit.hrl").

start() ->
	register(db_server, spawn(db_server, init, [])).

stop()->
	db_server ! stop,
	unregister(db_server),
	stop.

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
% db:module_info(attributes).


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

% db_server:stop().

some001_test_()->
	fun() -> ?assertEqual(true, true) end.

some002_test_()->
	[?_assertMatch(true, true)].

some003_test_()->
	{spawn,
		{setup
			,fun() -> compile:file(db), code:load_file(db) end % init
			%,fun(_) -> ?cmd("rm ./db_server.beam"), ?cmd("rm ./db.beam"), ?cmd("rm ./patches/001/db.beam"), ?cmd("rm ./patches/002/db.beam") end % clear
			,fun(_) -> ?cmd("rm /var/projects/test/erl/019/dbserver/db_server.beam"), ?cmd("rm /var/projects/test/erl/019/dbserver/db.beam"), ?cmd("rm /var/projects/test/erl/019/dbserver/patches/001/db.beam"), ?cmd("rm /var/projects/test/erl/019/dbserver/patches/002/db.beam") end % clear

			%test
			, {inorder,
				[
					some_test1()
					,some_test2()
					,some_test3()
				]
			}
		}
	}.

some_test1()->
	Value1 = true,
	Value2 = true,
	[
		?_assertEqual(true, true)
		,?_assertEqual(Value1, Value2)
	].

some_test2()->
	Value1 = true,
	Value2 = true,
	[
		?_assertEqual(true, true)
		,?_assertEqual(Value1, Value2)
	].

%test_load(Module, Path)->
%	case code:is_loaded(Module) of
%		{file, Path} -> {module, Module};
%		Any -> io:format("[~p]~n", [Any]),code:load_file(Module)
%	end.

some_test3()->
	Value1 = true,
	Value2 = true,
	[
		?_assertEqual(true, true)
		,?_assertEqual(Value1, Value2)

		,?_assertMatch(undefined, whereis(db_server))
		,?_assertEqual(true, db_server:start())
		,?_assertMatch(Pid when is_pid(Pid), whereis(db_server))

		,?_assertEqual([{vsn,[1.0]}], db:module_info(attributes))
		,?_assertMatch({write,francesco,sanfrancisco}, db_server:write(francesco, sanfrancisco))
		,?_assertMatch({write,alison, london}, db_server:write(alison, london))

		,?_assertEqual(ok, shell_default:cd("/var/projects/test/erl/019/dbserver/patches/001"))
		,?_assertEqual({ok,db}, compile:file(db))
		%,?_assertEqual({ok,db}, compile:file("/var/projects/test/erl/019/dbserver/patches/001/db.erl"))
		,?_assertEqual(true, code:add_patha("/var/projects/test/erl/019/dbserver/patches/001"))
		,?_assertEqual({module,db}, code:load_file(db))
		%,?_assertEqual({module,db}, test_load(db, "/var/projects/test/erl/019/dbserver/patches/001/db.beam"))
		,?_assertEqual(true, code:soft_purge(db))
		,?_assertEqual([{vsn,[1.1]}], db:module_info(attributes))

		,?_assertMatch({error,instance}, db_server:read(martin))
		,?_assertMatch({ok,london}, db_server:read(alison))
		,?_assertMatch({write, martin, cairo}, db_server:write(martin, cairo))
		,?_assertMatch({ok,cairo}, db_server:read(martin))

		,?_assertEqual(ok, shell_default:cd("/var/projects/test/erl/019/dbserver/patches/002"))
		,?_assertEqual({ok,db}, compile:file(db))
		%,?_assertEqual({ok,db}, compile:file("/var/projects/test/erl/019/dbserver/patches/002/db.erl"))
		,?_assertEqual(true, code:add_patha("/var/projects/test/erl/019/dbserver/patches/002"))
		,?_assertEqual({module,db}, code:load_file(db))
		%,?_assertEqual({module,db}, test_load(db, "/var/projects/test/erl/019/dbserver/patches/002/db.beam"))
		,?_assertEqual(true, code:soft_purge(db))
		,?_assertEqual([{vsn,[1.2]}], db:module_info(attributes))

		,?_assertEqual({upgrade,dict}, db_server:upgrade(dict))

		,?_assertMatch({write, test, test}, db_server:write(test, test))
		,?_assertMatch({ok,test}, db_server:read(test))

		,?_assertEqual(stop, db_server:stop())
		,?_assertMatch(undefined, whereis(db_server))

		,?_assertEqual(true, code:del_path("/var/projects/test/erl/019/dbserver/patches/001"))
		,?_assertEqual(true, code:del_path("/var/projects/test/erl/019/dbserver/patches/002"))

		,?_assertEqual(ok, shell_default:cd("/var/projects/test/erl/019/dbserver"))
		,?_assertEqual({module,db}, code:load_file(db))
		%,?_assertEqual({module,db}, test_load(db, "/var/projects/test/erl/019/dbserver/db.beam"))
		,?_assertEqual(true, code:soft_purge(db))
		,?_assertEqual([{vsn,[1.0]}], db:module_info(attributes))
	].

% c(db_server).
% db_server:test().
% eunit:test(db_server).
