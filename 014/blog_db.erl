-module(blog_db).
-include("blog.hrl").
-export([unixtime/0, unixtime/1, unixtime2timestamp/1, timestamp2datetime/1, timestamp/0, get_timestamp/0, micro2timestamp/1, micro2timestamp/1, datetime2unixstamp/1]).
-export([create/1, open/1, close/1]).
-export([add_item/2, add_item/3, delete_item/2, lookup_item/2, update_item/2, update_item/3]).
-export([lookup_free/1, get_all/1, get_simple/1]).

timestamp()->
	ErlangSystemTime = erlang:system_time(microsecond),
	MegaSecs = ErlangSystemTime div 1000000000000,
	Secs = ErlangSystemTime div 1000000 - MegaSecs*1000000,
	MicroSecs = ErlangSystemTime rem 1000000,
	{MegaSecs, Secs, MicroSecs}.

get_timestamp()->
	{Mega, Sec, Micro} = os:timestamp(),
	(Mega*1000000 + Sec)*1000 + round(Micro/1000).


micro2timestamp(microsecond)->
	M = 1000000,
	T = erlang:system_time(microsecond),
	{T div M div M, T div M rem M, T rem M}.

timestamp2datetime(Ts)->
	calendar:now_to_universal_time(Ts).

unixtime()->
	{Mega, Sec, Micro} = os:timestamp(),
	Mega*1000000 + Sec.

unixtime({Mega, Sec, Micro})->
	Mega*1000000 + Sec.

unixtime2timestamp(Value)->
	Mega = Value div 1000000,
	Sec = Value - Mega*1000000,
	{Mega, Sec, 0}.

gen_id()->
	get_timestamp().

datetime2unixstamp( {{Y,M,D},{Hr,Min,Sec}} = DT )->
	UnixSec = calendar:datetime_to_gregorian_seconds({{1970,01,01},{00,00,00}}), % 62167219200
	UnixSec = 62167219200,
	DtSec = calendar:datetime_to_gregorian_seconds(DT),
	DtSec - UnixSec.

create(Filename)->
	dets:open_file(tmp, [{file, Filename}, {keypos, #post.id}]), % {ok, Name} | {error, Reason}
	dets:close(tmp).

drop(Filename)->
	file:delete(Filename).

open(Filename)->
	% open_file(Filename) % {ok, Reference} | {error, Reason}
	Res = case dets:open_file(Filename) of 
		{ok, Reference} -> {ok, Reference};
		{error, Reason} -> {error, Reason}
	end,
	Res.

close(Ref) ->
	dets:close(Ref).



add_item(Ref, #post{id=Id, message=Message} = BlogPost)->
	case lookup_item(Ref, Id) of
		[] -> dets:insert(Ref, BlogPost#post{id=Id, stamp_created=unixtime(), stamp_last=unixtime(), message=Message});
		_ -> {error, already_exists}
	end;

add_item(Ref, Message)->
	%Id = lookup_free(Ref),
	Id = gen_id(),
	case add_item(Ref, #post{id=Id, message=Message}) of 
		ok -> {ok, Id};
		{error, Reason} -> {error, Reason}
	end.

add_item(Ref, Message, Date)->
	%Id = lookup_free(Ref),
	Id = gen_id(),
	case add_item(Ref, #post{id=Id, message=Message, stamp_created=Date}) of 
		ok -> {ok, Id};
		{error, Reason} -> {error, Reason}
	end.

delete_item(Ref, Key)->
	dets:delete(Ref, Key).

update_item(Ref, #post{id=Id, stamp_created=Created, stamp_last=Last, message=Message, status=Status} = BlogPost)->
	case lookup_item(Ref, Id) of
		[] -> {error, not_found};
		[Post] -> dets:insert(Ref, Post#post{stamp_last=unixtime(), message=Message, status=Status})
	end.

update_item(Ref, Id, Message)->
	case update_item(Ref, #post{id=Id, stamp_created=unknown, stamp_last=unknown, message=Message, status=enabled}) of 
		ok -> {ok, Id};
		{error, Reason} -> {error, Reason}
	end.

lookup_item(Ref, Id)->
	dets:lookup(Ref, Id).

lookup_one(Ref, Id)->
	[Res] = dets:lookup(Ref, Id),
	Res.

lookup_free(Ref)->
	lookup_free(Ref, 1).

lookup_free(Ref, Id)->
	case dets:lookup(Ref, Id) of
		[] -> Id;
		_ -> lookup_free(Ref, Id+1)
	end.

get_all(Ref)->
	case dets:first(Ref) of
		'$end_of_table' -> [];
		%Key -> [Key] ++ get_all(Ref, Key)
		Key -> [lookup_one(Ref, Key)] ++ get_all(Ref, Key)
	end.

get_all(Ref, KeyCurrent)->
	case dets:next(Ref, KeyCurrent) of
		'$end_of_table' -> [];
		%Key -> [Key] ++ get_all(Ref, Key)
		Key -> [lookup_one(Ref, Key)] ++ get_all(Ref, Key)
	end.

get_simple(Ref)->
	Data = get_all(Ref),
	smplefy(Data).

smplefy([])->
	[];

smplefy([H|[]])->
	%[{H#post.id, H#post.stamp_created, H#post.message}];
	Ut = H#post.stamp_created,
	Ts = blog_db:unixtime2timestamp(Ut),
	Dt = blog_db:timestamp2datetime(Ts),
	[{H#post.id, Dt, H#post.message}];

smplefy([H|T])->
	%[{H#post.id, H#post.stamp_created, H#post.message}] ++ smplefy(T);
	Ut = H#post.stamp_created,
	Ts = blog_db:unixtime2timestamp(Ut),
	Dt = blog_db:timestamp2datetime(Ts),
	[{H#post.id, Dt, H#post.message}] ++ smplefy(T);

smplefy(Any)->
	{error, smplefy}.

% c(blog_db).

% Ut = blog_db:unixtime().
% Ts = blog_db:unixtime2timestamp(Ut).
% Dt = blog_db:timestamp2datetime(Ts).

% Ts = erlang:timestamp().
% Dt = calendar:now_to_datetime(Ts).
% Ts = calendar:datetime_to_gregorian_seconds(Dt).

% blog_db:create("DB1").
% {ok, T1} = blog_db:open("DB1").
% blog_db:get_all(T1).
% blog_db:get_simple(T1).
% blog_db:lookup_free(T1).
% blog_db:add_item(T1, "Test message!").
% blog_db:lookup_item(T1, 2).
% blog_db:update_item(T1, 2, "Test message!").
% blog_db:delete_item(T1, 1).
% blog_db:close(T1).