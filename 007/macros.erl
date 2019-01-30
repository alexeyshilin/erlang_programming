-module(macros).
-export([test/0]).
%-include("macros.hrl").

-define(
	SHOW_EVAL(Expr, Mode), 
	if Mode == show ->
			io:format("[~p = ~p]~n", [??Expr,Expr]);
		not(Mode == show) -> 
			io:format("[~p]~n", [Expr])
	end
).

test()->
	?SHOW_EVAL(length([1,2,3]), show),
	?SHOW_EVAL(length([1,2,3]), none),
	ok.

% c(macros).
% macros:test().
