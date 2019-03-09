-module(my_db_gen).
-export([test/0]).
-export([start_link/0, start_link/1, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).
-export([start/0, stop/0, write/2, delete/1, read/1, match/1]).
-behavior(genserver).


% 12-1

% Client's functions
% Tech API

start_link() ->
	start_link([]).

start_link(InitData) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, InitData, []).

stop_link() ->
	gen_server:cast(?MODULE, stop).

% Client API

%my_db_gen:start() => ok.
%my_db_gen:stop() => ок.
%my_db_gen:write(Key,Element) => ok.
%my_db_gen:delete(Key) => ok.
%my_db_gen:read(Key) => {ok,Element}|{error,instance}.
%my_db:match(Element) => [Keyl, ..., KeyN].

start() ->
	start_link().

start(Data) ->
	start_link(Data).

stop() ->
	stop_link().

write(Key,Element) ->
	gen_server:call(?MODULE, {write, Key, Element}).

delete(Key) ->
	gen_server:call(?MODULE, {delete, Key}).

read(Key) ->
	gen_server:call(?MODULE, {read, Key}).

match(Element) ->
	gen_server:call(?MODULE, {match, Element}).

% service app API


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

% callback functions

init(InitData) ->
	{ok, InitData}.

terminate(_Reason, _LoopData) ->
	{ok}.

handle_cast(stop, LoopData) ->
	{stop, normal, LoopData}.

handle_call({test, Param1, Param2}, From, LoopData) ->
	Reply = {ok, some_data},
	NewLoopData = LoopData,
	{reply, Reply, NewLoopData};

handle_call({write, Key, Element}, From, LoopData) ->
	NewLoopData = write(Key, Element, LoopData),
	Reply = {ok},
	{reply, Reply, NewLoopData};

handle_call({delete, Key}, From, LoopData) ->
	NewLoopData = delete(Key, LoopData),
	Reply = {ok},
	{reply, Reply, NewLoopData};

handle_call({read, Key}, From, LoopData) ->
	Reply = read(Key,LoopData),
	{reply, Reply, LoopData};

handle_call({match, Element}, From, LoopData) ->
	Reply = match(Element, LoopData),
	{reply, Reply, LoopData};

handle_call(stub, From, LoopData) ->
	{reply, stub, LoopData}.

% Tests

test()->
	{ok, Pid} = my_db_gen:start(),
	{ok} = my_db_gen:write(foo, bar),
	{error, instance} = my_db_gen:read(baz),
	{ok, bar} = my_db_gen:read(foo),
	[foo] = my_db_gen:match(bar),
	ok = stop(),
	ok.

% /Tests

% c(my_db_gen).
%
% my_db_gen:test().
