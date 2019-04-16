-module(list).
%-export([test/0]).
-include_lib("eunit/include/eunit.hrl").

create(C, L) when C==L ->
	[C];

create(C, L) when C<L ->
	[C | create(C+1, L)].

create(L) when L >= 0 ->
	create(1, L).

reverse_create(L) when L > 0 ->
	[L | reverse_create(L-1)];

reverse_create(L) when L==0 ->
	[].

%test()->
%	[1,2,3] = create(3),
%	[3,2,1] = reverse_create(3),
%	ok.

create_test()->
	?assertEqual([1,2,3], create(3)).

reverse_create_test()->
	?assertEqual([3,2,1], reverse_create(3)).

some_test()->
	?assert( 1 =:= 1 ),
	?assert( not(1 =:= 2) ),
	?assertEqual(1, 1),
	?assertNot( 1 =:= 2 ),
	?assertMatch({some, Val} when is_integer(Val), {some, 101}),
	?assertExit({reason, 123}, exit({reason, 123})),
	?assertError(badarith, 1/0),
	?assertThrow(some, throw(some)).

% c(list).
%
% list:test().
