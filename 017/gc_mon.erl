-module(gc_mon).
-export([test/0]).
-export([measure/3, gc_test/2]).
-export([measure_loop/3, ts2mili/1, ts2micro/1]).
-export([average_tr/1, average_ntr/1]).

% tail-recursive function
average_tr(List) -> average_acc(List, 0, 0 ).
average_acc([], Sum, Length) -> Sum / Length;
average_acc([H | T], Sum, Length) -> average_acc(T, Sum + H, Length + 1).

% non-tail-recursive function
average_ntr(List) -> sum(List) / len(List).

sum([]) -> 0;
sum([Head | Tail]) -> Head + sum(Tail).

len([]) -> 0;
len([_ | Tail]) -> 1 + len(Tail).

gc_test(Fun, List)->
	%apply(Fun, Args)
	%apply(Module, Function, Args)

	%erlang:apply(Fun, [List]).
	%erlang:apply(gc_mon, Fun, [List]).
	loop(Fun, List).

loop(Fun, List)->
	receive
		{From, start} ->
			erlang:apply(gc_mon, Fun, [List]),
			ok;
		Any ->
			{error, loop, Any}
	end.

measure(Param1, Param2, Param3)->
	Pid = spawn(gc_mon, Param1, [Param2, Param3]),
	Pid2 = spawn(gc_mon, measure_loop, [Pid, null, null]),
	
	erlang:trace(Pid, true, [garbage_collection, timestamp, {tracer, Pid2}]),
	%Pid2 ! {self(), start},
	
	Pid ! {self(), start},
	erlang:garbage_collect(Pid),

	Pid2 ! {self(), result},
	
	receive
		{Pid2, result, Val} ->
			io:format("[Gc monitoring terminated]\n"),
			io:format("[Microseconds: ~p]\n", [Val]),
			ok
		after 3000 ->
			io:format("[timeout]"),
			timeout
	end,
	ok.

ts2mili({Mega, Sec, Micro}) ->
	(Mega*1000000 + Sec)*1000 + round(Micro/1000).

ts2micro({Mega, Sec, Micro}) ->
	(Mega*1000000 + Sec)*1000000 + Micro.

% gc_start gc_end
% gc_major_start gc_major_end
% gc_minor_start gc_minor_end
measure_loop(Pid, Start, End) ->
	receive
		{From, start} ->
			io:format("[Start: ~p]\n", [Pid]),
			erlang:trace(Pid, true, [garbage_collection, timestamp]),
			%erlang:trace(Pid, true, [{tracer, self()}]),
			ok;
		{trace_ts, Pid, gc_start, Data, Ts} ->
			io:format("[gc_start: ~p]\n", [Ts]),
			measure_loop(Pid, Start, End);
		{trace_ts, Pid, gc_end, Data, Ts} ->
			io:format("[gc_end: ~p]\n", [Ts]),
			measure_loop(Pid, Start, End);
		{trace_ts, Pid, gc_minor_start, Data, Ts} ->
			io:format("[gc_minor_start: ~p]\n", [Ts]),
			measure_loop(Pid, Start, End);
		{trace_ts, Pid, gc_minor_end, Data, Ts} ->
			io:format("[gc_minor_end: ~p]\n", [Ts]),
			measure_loop(Pid, Start, End);

		{trace_ts, Pid, gc_major_start, Data, Ts} ->
			io:format("[gc_major_start: ~p]\n", [Ts]),
			{Megaseconds, Seconds, Microseconds} = Ts,
			%Data: mbuf_size, recent_size, stack_size, old_heap_size, heap_size
			measure_loop(Pid, Ts, End);
		{trace_ts, Pid, gc_major_end, Data, Ts} ->
			io:format("[gc_major_end: ~p]\n", [Ts]),
			{Megaseconds, Seconds, Microseconds} = Ts,
			%Data: mbuf_size, recent_size, stack_size, old_heap_size, heap_size
			measure_loop(Pid, Start, Ts);

		{From, result} ->
			io:format("[~p ~p]\n", [Start, End]),
			Res = ts2micro(End) - ts2micro(Start),
			From ! {self(), result, Res},
			ok;
		Any ->
			io:format("[Error: ~p]\n", [Any]),
			{error, measure_loop, Any}
	end.

test()->
	List = lists:seq(1, 1000),
	Res = average_tr(List),
	Res = average_ntr(List),

	ok.

% c(gc_mon).
%
% gc_mon:test().
%
% gc_mon:ts2mili(os:timestamp()).
% gc_mon:ts2micro(os:timestamp()).
% Ts = os:timestamp().
% gc_mon:ts2mili(Ts).
% gc_mon:ts2micro(Ts).

% List = lists:seq(1, 1000).
% gc_mon:measure(gc_test, average_tr, List).
