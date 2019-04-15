-module(sideeffect).
-export([test/0]).

print(N) when is_integer(N) ->
	print(1, N).

print(C, N) when C =< N ->
	io:format("Number: ~p~n", [C]),
	print(C+1, N);

print(C, N) when C > N ->
	done.

print_odd(N) when is_integer(N)->
	print_odd(1, N).

print_odd(C, N) when (C=<N) and (C rem 2 > 0) ->
	io:format("Number: ~p~n", [C]),
	print_odd(C+1, N);

print_odd(C, N) when (C=<N) ->
	print_odd(C+1, N);

print_odd(C, N) when (C>N) ->
	done.

test()->
	done = print(5),
	done = print_odd(5),
	ok.

% c(sideeffect).
%
% sideeffect:test().
