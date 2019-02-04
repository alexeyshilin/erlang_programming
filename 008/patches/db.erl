-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, convert/2]).
-export([code_upgrade/1,code_upgrade/2]).
-vsn(1.3).

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

% code_upgrade
%db:code_upgrade([RecordList]) -> gb_tree().
code_upgrade([{Key,Value}|RecordList]) ->
	db:code_upgrade([{Key,Value}|RecordList], new()).

code_upgrade([], GbTree) ->
	GbTree;

code_upgrade([{Key,Value}|[]], GbTree) ->
	 NewGbTree = gb_trees:insert(Key, Value, GbTree),
	 code_upgrade([], NewGbTree);

code_upgrade([{Key,Value}|RecordList], GbTree) ->
	 NewGbTree = gb_trees:insert(Key, Value, GbTree),
	 code_upgrade(RecordList, NewGbTree).
% /code_upgrade


% cd("/var/projects/test/erl/003").
% compile:file(db).
% code:load_file(db).
% db:module_info().
%
% Db = db:new().
% Db1 = db:write(francesco, london, Db).
% Db2 = db:write(lelle, "Stockholm", Db1).
% Db3 = db:write(joern, 'Stockholm', Db2).
%
% cd("/var/projects/test/erl/008/patches").
% compile:file(db).
% code:load_file(db).
% code:soft_purge(db).
% db:module_info().
% db:code_upgrade(Db3).

%%{3,
%% {francesco,london,nil,
%%            {lelle,"Stockholm",{joern,'Stockholm',nil,nil},nil}}}
