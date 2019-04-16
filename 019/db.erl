-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).
-include_lib("eunit/include/eunit.hrl").

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


db_001_test_()->
	fun() -> ?assertEqual([], db:new()) end.

db_002_test_()->
	?_assertEqual([], db:new()).

somegroup_test_()->
	[
	?_assertEqual([], db:new())
	,?_assertEqual(ok, db:destroy([]))
	].

some_test_()->
	[test_one(), test_two(), test_three()].

test_one()->
	[
	?_assertEqual([], db:new())
	,?_assertEqual(ok, db:destroy([]))
	].

test_two()->
	[
	?_assertEqual([], db:new())
	,?_assertEqual(ok, db:destroy([]))
	].

test_three()->
	[
	?_assertEqual([], db:new())
	,?_assertEqual(ok, db:destroy([]))
	].

full_001_test_()->
	Db = db:new(),
	{spawn,
		{setup,
			fun()-> DbTest = db:new() end,
			fun()-> ok = db:destroy(Db) end,
			?_assertEqual([{francesco,london}], db:write(francesco, london, Db))
		}
	}.

full_002_test_()->
	Db0 = db:new(),
	Db = db:write(francesco, london, Db0),

	{spawn,
		{setup,
			fun()-> _ = db:new() end,
			fun()-> ok = db:destroy(Db) end,
			[
			?_assertEqual([{lelle,'Stockholm'},{francesco,london}], db:write(lelle, 'Stockholm', Db))
			,?_assertEqual({ok,london}, db:read(francesco, Db))
			]
		}
	}.

% c(db).
%
% db:test().
%
% eunit:test(db).
% eunit:test({db, somegroup_test_}).
% eunit:test({db, some_test_}).
% eunit:test({generator, fun db:some_test_/0}).
% eunit:test({dir,"./"}).

% eunit:test({spawn, db}).
% eunit:test({timeout, 10, db}).
% eunit:test({timeout, 10, {db, somegroup_test_}}).
% eunit:test({timeout, 10, {inorder, {db, some_test_}}}).
% eunit:test({timeout, 10, {inparallel, {db, some_test_}}}).

% eunit:test({db, full_001_test_}).
% eunit:test({db, full_002_test_}).
