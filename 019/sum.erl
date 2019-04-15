-module(sum).
%-export([test/0]).
-include_lib("eunit/include/eunit.hrl").

%-define(NOTEST, true).

sum(0)->
	0;

sum(Val) when is_integer(Val)->
	Val + sum(Val-1).

sum(S, F) when S==F ->
	F;

sum(S, F)->
	S + sum(S+1, F).

%test()->
%	15 = sum(5),
%	6 = sum(1,3),
%	6 = sum(6,6),
%
%	ok.

sum_test()->
	?assertEqual(15, sum(5)).

sum2_test()->
	?assertEqual(6, sum(1,3)),
	?assertEqual(6, sum(6,6)).

sum2err_test()->
	?assertError({badmatch,_}, 10 = sum(1,3)),

	?assertError(badarith, 1/0),
	?assertThrow(some, throw(some)).

% c(sum).
%
% sum:test().
