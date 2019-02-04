-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, convert/2]).
-vsn(1.2).

new() -> gb_trees:empty().

write(Key, Data, Db) -> gb_trees:insert(Key, Data, Db).

read(Key, Db) ->
	case gb_trees:lookup(Key, Db) of
		none -> {error, instance};
		{value, Data} -> {ok, Data}
	end.

destroy(Db) -> ok.

delete(Key, Db) -> gb_trees:delete(Key, Db).

convert(dict, Dict) ->
	%dict(dict:fetch_keys(Dict), Dict, new());
	Keys = dict:fetch_keys(Dict),
	New = new(),
	%io:format("[Keys: ~p]", [Keys]),
	%io:format("[Dict: ~p]", [Dict]),
	%io:format("[New: ~p]", [New]),
	Res = dict(Keys, Dict, New),
	%io:format("[Res: ~p]", [Res]),
	Res;

convert(_, Data) ->
	Data.

dict([Key|Tail], Diet, GbTree) ->
	Data = dict:fetch(Key, Diet),
	NewGbTree = gb_trees:insert(Key, Data, GbTree),
	dict(Tail, Diet, NewGbTree);

dict([], _, GbTree) -> GbTree.

% mkdir /var/projects/test/erl/008/patches
% cd /var/projects/test/erl/008/patches
% erl
% c(db).
% cd ..


% c(db).
%
% cd('/var/projects/test/erl/008").
% make:all([load]).
% db:module_info().
% db_server:start().
% db_server:write(francesco, sanfrancisco).
% db_server:write(alison, london).
% db_server:read(alison).
% db_server:read(martin).
% code:add_patha("/var/projects/test/erl/008/patches").
	%%compile:file(db). % file ./patches/db.erl must be already compiled
% code:load_file(db).
% code:soft_purge(db).
% db_server:upgrade(dict).
% db:module_info().
% db_server:write(martin, cairo) .
% db_server:read(francesco).
% db_server:read(martin).
%