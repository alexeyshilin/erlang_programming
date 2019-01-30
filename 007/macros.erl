-module(macros).
-export([test/0]).
-export([some_func/1]).
%-include("macros.hrl").

-define(
	SHOW_EVAL(Expr, Mode), 
	if Mode == show ->
			io:format("[~p = ~p]~n", [??Expr,Expr]);
		not(Mode == show) -> 
			io:format("[~p]~n", [Expr])
	end
).

-define(debug, ok).

-ifdef(debug).
	-define(CALLS(Func), io:format("[call ~p]~n", [??Func])).
-else.
	-define(CALLS(Func), ok).
-endif.


some_func(Param)->
	?CALLS(some_func(Param)),
	io:format("[test]"),
	ok.

test()->
	?SHOW_EVAL(length([1,2,3]), show),
	?SHOW_EVAL(length([1,2,3]), none),
	some_func(111),
	some_func(222),
	ok.

% c(macros).
% macros:test().
