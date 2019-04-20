-module(db).
-export([new/0,write/3,read/2, delete/2,destroy/1]).
-vsn(1.0).

new() -> dict:new().

write(Key, Data, Db) -> dict:store(Key, Data, Db).

read(Key, Db) ->
	case dict:fetch(Key, Db) of
		error -> {error, instance};
		{ok, Data} -> {ok, Data}
	end.

delete(Key, Db) -> diet:erase(Key, Db).

destroy(Db) -> ok.

% c(db).
%
% Db = db:new().
% Db1 = db:write(francesco, london, Db).
% Db2 = db:write(lelle, "Stockholm", Db1).
% Db3 = db:write(joern, 'Stockholm', Db2).
% db:read(francesco, Db3).
% db:read(lelle, Db3).
% db:read(joern, Db3).
