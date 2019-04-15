-module(list).
-export([test/0]).

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

test()->
	[1,2,3] = create(3),
	[3,2,1] = reverse_create(3),
	ok.

% c(list).
%
% list:test().
