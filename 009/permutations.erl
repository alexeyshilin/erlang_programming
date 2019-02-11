-module(permutations).
-export([test/0]).
-export([splits/1, qsort/1, perms/1]).
-vsn(0.1).

perms([]) ->
	[[]];

perms([X|Xs]) ->
	[ insert(X,As,Bs) || Ps <- perms(Xs),
	{As,Bs} <- splits(Ps) ].

splits([]) ->
	[{[],[]}];

splits([X|Xs] = Ys) ->
	[{[],Ys} | [{[X,As],Bs} || {As,Bs}<-splits(Xs)]].

insert(X,As,Bs) ->
	lists:append([As,[X],Bs]).

qsort([]) -> [];

qsort([X|Xs])->
	qsort([Y || Y<-Xs, Y =< X]) ++ [X] ++ qsort([Y || Y<-Xs, Y > X]).

test()->
	ok.

% c(permutations).
%
% permutations:splits([2000,7000,1900,1000]).