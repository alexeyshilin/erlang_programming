-module(redirection).
-export([test/0]).

test()->
	HandlerFun =
		fun({trace, Pid, gc_minor_start, Start}, _) ->
				Start;
			({trace, Pid, gc_major_start, Start}, _) ->
				Start;
			({trace, Pid, gc_minor_end, End}, Start) ->
				{_, {_,OHS}} = lists:keysearch(old_heap_size, 1, Start),
				{_, {_,OHE}} = lists:keysearch(old_heap_size, 1, End),
				io:format("Old heap size delta after gc:~w~n", [OHS-OHE]),
				{_, {_,HS}} = lists:keysearch(heap_size, 1, Start),
				{_, {_,HE}} = lists:keysearch(heap_size, 1, End),
				io:format("Heap size delta after gc:~w~n", [HS-HE]);
			({trace, Pid, gc_major_end, End}, Start) ->
				{_, {_,OHS}} = lists:keysearch(old_heap_size, 1, Start),
				{_, {_,OHE}} = lists:keysearch(old_heap_size, 1, End),
				io:format("Old heap size delta after gc:~w~n", [OHS-OHE]),
				{_, {_,HS}} = lists:keysearch(heap_size, 1, Start),
				{_, {_,HE}} = lists:keysearch(heap_size, 1, End),
				io:format("Heap size delta after gc:~w~n", [HS-HE]);

			({trace, Pid, gc_start, Start} , _) ->
				Start;
			({trace, Pid, gc_end, End}, Start) ->
				{_, {_,OHS}} = lists:keysearch(old_heap_size, 1, Start),
				{_, {_,OHE}} = lists:keysearch(old_heap_size, 1, End),
				io:format("Old heap size delta after gc:~w~n", [OHS-OHE]),
				{_, {_,HS}} = lists:keysearch(heap_size, 1, Start),
				{_, {_,HE}} = lists:keysearch(heap_size, 1, End),
				io:format("Heap size delta after gc:~w~n", [HS-HE])
		end,

	dbg:tracer(process, {HandlerFun, null}),

	dbg:p(self(), [garbage_collection]),

	List = lists:seq(1,1000),

	io:format("~p", [List]),

	RevList = lists:reverse(List),

	io:format("~p", [RevList]),

	ok.

% c(redirection).
% redirection:test().
