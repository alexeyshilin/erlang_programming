-module(gc_mon_dbg).
-export([test/0]).
-export([measure/3, gc_test/2]).
-export([ts2mili/1, ts2micro/1]).
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
	%erlang:apply(gc_mon_dbg, Fun, [List]).
	loop(Fun, List).

loop(Fun, List)->
	receive
		{From, start} ->
			erlang:apply(gc_mon_dbg, Fun, [List]),
			ok;
		Any ->
			{error, loop, Any}
	end.

print_item(Start, End, Item) ->
	{_, {_,S}} = lists:keysearch(Item, 1, Start),
	{_, {_,E}} = lists:keysearch(Item, 1, End),
	io:format("~p:~w~n", [Item, S-E]).

print_diff(Start, End) ->
%wordsize, old_heap_block_size, heap_block_size, mbuf_size, recent_size, stack_size, old_heap_size, heap_size, bin_vheap_size, bin_vheap_block_size, bin_old_vheap_size, bin_old_vheap_block_size
	io:format("~nAll values:~n"),
	print_item(Start, End, wordsize),
	print_item(Start, End, old_heap_block_size),
	print_item(Start, End, heap_block_size),
	print_item(Start, End, mbuf_size),
	print_item(Start, End, recent_size),
	print_item(Start, End, stack_size),
	print_item(Start, End, old_heap_size),
	print_item(Start, End, heap_size),
	print_item(Start, End, bin_vheap_size),
	print_item(Start, End, bin_vheap_block_size),
	print_item(Start, End, bin_old_vheap_size),
	print_item(Start, End, bin_old_vheap_block_size),
	ok.

measure(Param1, Param2, Param3)->

	HandlerFun =
		fun({trace_ts, Pid, gc_minor_start, Start, Ts}, _) ->
				{Start, Ts};
			({trace_ts, Pid, gc_major_start, Start, Ts}, _) ->
				{Start, Ts};

			({trace_ts, Pid, gc_minor_end, End, Ts}, {Start,StartTs}) ->
				Res = ts2micro(Ts) - ts2micro(StartTs),
				io:format("[Gc monitoring terminated]\n"),
				io:format("[Microseconds: ~p]\n", [Res]),

				{_, {_,OHS}} = lists:keysearch(old_heap_size, 1, Start),
				{_, {_,OHE}} = lists:keysearch(old_heap_size, 1, End),
				io:format("Old heap size delta after gc:~w~n", [OHS-OHE]),
				{_, {_,HS}} = lists:keysearch(heap_size, 1, Start),
				{_, {_,HE}} = lists:keysearch(heap_size, 1, End),
				io:format("Heap size delta after gc:~w~n", [HS-HE]),

				print_diff(Start, End);


			({trace_ts, Pid, gc_major_end, End, Ts}, {Start,StartTs}) ->
				Res = ts2micro(Ts) - ts2micro(StartTs),
				io:format("[Gc monitoring terminated]\n"),
				io:format("[Microseconds: ~p]\n", [Res]),

				{_, {_,OHS}} = lists:keysearch(old_heap_size, 1, Start),
				{_, {_,OHE}} = lists:keysearch(old_heap_size, 1, End),
				io:format("Old heap size delta after gc:~w~n", [OHS-OHE]),
				{_, {_,HS}} = lists:keysearch(heap_size, 1, Start),
				{_, {_,HE}} = lists:keysearch(heap_size, 1, End),
				io:format("Heap size delta after gc:~w~n", [HS-HE]),

				print_diff(Start, End);

			({trace_ts, Pid, gc_start, Start, Ts} , _) ->
				{Start, Ts};

			({trace_ts, Pid, gc_end, End, Ts}, {Start,StartTs}) ->
				Res = ts2micro(Ts) - ts2micro(StartTs),
				io:format("[Gc monitoring terminated]\n"),
				io:format("[Microseconds: ~p]\n", [Res]),

				{_, {_,OHS}} = lists:keysearch(old_heap_size, 1, Start),
				{_, {_,OHE}} = lists:keysearch(old_heap_size, 1, End),
				io:format("Old heap size delta after gc:~w~n", [OHS-OHE]),
				{_, {_,HS}} = lists:keysearch(heap_size, 1, Start),
				{_, {_,HE}} = lists:keysearch(heap_size, 1, End),
				io:format("Heap size delta after gc:~w~n", [HS-HE]),

				print_diff(Start, End)
		end,
	
	Pid = spawn(gc_mon_dbg, Param1, [Param2, Param3]),

	dbg:tracer(process, {HandlerFun, null}),

	dbg:p(Pid, [garbage_collection, timestamp]),
	
	Pid ! {self(), start},
	erlang:garbage_collect(Pid),

	ok.

ts2mili({Mega, Sec, Micro}) ->
	(Mega*1000000 + Sec)*1000 + round(Micro/1000).

ts2micro({Mega, Sec, Micro}) ->
	(Mega*1000000 + Sec)*1000000 + Micro.


test()->
	List = lists:seq(1, 1000),
	Res = average_tr(List),
	Res = average_ntr(List),

	ok.

% c(gc_mon_dbg).
%
% gc_mon_dbg:test().
%
% gc_mon_dbg:ts2mili(os:timestamp()).
% gc_mon_dbg:ts2micro(os:timestamp()).
% Ts = os:timestamp().
% gc_mon_dbg:ts2mili(Ts).
% gc_mon_dbg:ts2micro(Ts).

% List = lists:seq(1, 1000).
% gc_mon_dbg:measure(gc_test, average_tr, List).
