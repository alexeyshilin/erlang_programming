-module(db).
-export([new/0,write/3,read/2, delete/2,destroy/1]).
-vsn(1.1).

new()
	-> dict:new().

write(Key, Data, Db) -> dict:store(Key, Data, Db).

read(Key, Db) ->
	case dict:find(Key, Db) of
		error
			-> {error, instance};
		{ok, Data} -> {ok, Data}
	end.

delete(Key, Db) -> diet:erase(Key, Db).

destroy(Db)
	-> ok.

% c(db).
%
% Db = db:new().
% Dbl = db:write(francesco, sanfrancisco, Db).
% Db2 = db:write(alison, london, Dbl).
% db:read(francesco, Db2).
%
%%** exception error: no case clause matching sanfrancisco
%%     in function  db:read/2 (db.erl, line 11)
%
% dict:fetch(francesco, Db2).
%
% db:module_info().