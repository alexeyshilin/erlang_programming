-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

% 3-4

% https://github.com/jordillonch/Erlang-Programming-exercises/blob/master/db.erl

% db:new() => Db.
% db:destroy(Db) => ok.
% db:write(Key, Element, Db) => NewDb.
% db:delete(Key, Db) => NewDb.
% db:read(Key, Db) => {ok, Element} | {error, instance}.
% db:match(Element, Db) => [Keyl, ..., KeyN].



% 1> c(db).
% {ok,db}
% 2> Db = db:new().
% []
% 3> Dbl = db:write(francesco, london, Db).
% [{francesco,london}]
% 4> Db2 = db:write(lelle, Stockholm, Dbl).
% 4> Db2 = db:write(lelle, 'Stockholm', Dbl).
% [{lelle,Stockholm},{francesco,london}]
% 5> db:read(francesco, Db2).
% {ok,london}
% 6> Db3 = db:write(joern, Stockholm, Db2).
% 6> Db3 = db:write(joern, 'Stockholm', Db2).
% [{joern,Stockholm},{lelle,Stockholm},{francesco,london}]
% 7> db:read(ola, Db3).
% {error,instance}
% 8> db:match(stockholm, Db3).
% 8> db:match('Stockholm', Db3).
% [joern,lelle]
% 9> Db4 = db:delete(lelle, Db3).
% [{joern,Stockholm},{francesco,london}]
% 10> db:match(stockholm, Db4).
% [joern]
% 11>

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

