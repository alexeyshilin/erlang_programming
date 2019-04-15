-module(sum).
-export([test/0]).

sum(0)->
	0;

sum(Val) when is_integer(Val)->
	Val + sum(Val-1).

sum(S, F) when S==F ->
	F;

sum(S, F)->
	S + sum(S+1, F).

test()->
	15 = sum(5),
	6 = sum(1,3),
	6 = sum(6,6),

	ok.

% c(sum).
%
% sum:test().
