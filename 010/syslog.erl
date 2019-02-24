-module(syslog).
-export([test/0]).
-export([syslog/0]).
-include("syslog.hrl").
-vsn(-0).

mynow()->
	%erlang:timestamp().
	erlang:system_time().

open() ->
	Id = ets:new(indexTable, [ordered_set, named_table]),
	Id.

close(Id)->
	ets:delete(Id).

delete(Id)->
	ets:delete(Id).

msg_sent(Data, Ts)->
	ets:insert(indexTable, {Ts, Data, null}),
	Ts.

msg_ack(Ts, TsRecv)->
	[{Ts,Data, null}] = ets:lookup(indexTable, Ts),
	ets:insert(indexTable, {Ts, Data, TsRecv}).


check_unack(Entry) when Entry==null ->
	First = ets:first(indexTable),
	case First of
		'$end_of_table' -> ok;
		_ ->
			[{Ts, Data, TsRecv}] = ets:lookup(indexTable, First),
			if TsRecv == null ->
				io:format("[unack: ~p]",[Data]);
				not(TsRecv == null) ->
					ok
			end,
			check_unack(First)
	end,
	ok;

check_unack(Entry) when not(Entry==null) ->
	Next = ets:next(indexTable, Entry),
	case Next of
		'$end_of_table' -> ok;
		_ ->
			[{Ts, Data, TsRecv}] = ets:lookup(indexTable, Next),
			if TsRecv == null ->
				io:format("[unack: ~p]",[Data]);
				not(TsRecv == null) ->
					ok
			end,
			check_unack(Next)
	end,
	ok;

check_unack(Other) ->
	error.

check_avg() ->
	{Time,Count} = check_avg(null, {null,null}),
	(erlang:convert_time_unit(Time, native, nanosecond))/(1000000000*Count).

check_avg(Entry, {null, null}) when Entry==null ->
	First = ets:first(indexTable),
	case First of
		'$end_of_table' -> {0,0};
		_ ->
			[{Ts, Data, TsRecv}] = ets:lookup(indexTable, First),
			if TsRecv == null -> check_avg(First, {0, 0});
				not(TsRecv == null) -> check_avg(First, {TsRecv-Ts, 1})
			end
	end;

check_avg(Entry, {Time, Count}) when not(Entry==null) ->
	Next = ets:next(indexTable, Entry),
	case Next of
		'$end_of_table' -> {Time, Count};
		_ ->
			[{Ts, Data, TsRecv}] = ets:lookup(indexTable, Next),
			if TsRecv == null -> check_avg(Next, {Time, Count});
				not(TsRecv == null) -> check_avg(Next, {TsRecv-Ts, Count+1})
			end
	end.

syslog()->
	ok.

test()->
	Syslog = open(),

	Id1 = msg_sent("Test 1", mynow()),
	timer:sleep(1),
	Id2 = msg_sent("Test 2", mynow()),
	timer:sleep(10),
	Id3 = msg_sent("Test 3", mynow()),
	timer:sleep(100),
	Id4 = msg_sent("Test 4", mynow()),
	timer:sleep(200),
	Id5 = msg_sent("Test 5", mynow()),
	timer:sleep(10),

	msg_ack(Id1, mynow()),
	timer:sleep(100),
	msg_ack(Id2, mynow()),
	timer:sleep(100),
	msg_ack(Id3, mynow()),
	timer:sleep(10),
	msg_ack(Id4, mynow()),

	check_unack(null),
	Ret = check_avg(),
	io:format("[avg: ~p]",[Ret]),

	%close(Syslog),

	delete(Syslog),

	ok.

% c(syslog).
%
% syslog:test().
%