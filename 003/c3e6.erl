-module(c3e6).
-export([sort_fast/1, sort_merge/1]).

% 3-6

sort_fast([Head,Tail])->
	ok.

greater_and_lower(Val, [Head, Tail], Lower, Greater )->
	if
		(Head < Tail) -> Lower2 = [Head|Lower];
		(Head > Tail) or (Head == Tail) -> Greater2 = [Head|Greater];

sort_merge()->
	ok.
