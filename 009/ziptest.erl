-module(ziptest).
-export([test/0]).
-export([zip/2]).
-vsn(-0).

zip([],[])->
	[];

zip([H1|[T1]],[H2|[]])->
	[{H1,H2}];

zip([H1|[]],[H2|T2])->
	[{H1,H2}];

zip([H1|T1],[H2|T2])->
	[{H1,H2}] ++ zip(T1,T2);

zip([H1|_],[])->
	[];

zip([],[H2|_])->
	[];

zip(A,B)->
	exit(error).


zipWith(F, [],[])->
	[];

zipWith(F, L1, L2)->
	L3 = zip(L1, L2),
	[F(X,Y) || {X,Y}<-L3].

test()->
	%Res1 = zip([1,2], [3,4,5]),
	%Res1.
	[{1,3},{2,4}] = zip([1,2], [3,4,5]),

	Add = fun(X,Y)->
		X+Y
	end,

	%Res2 = zipWith(Add, [1,2], [3,4,5]),
	%Res2.
	[4,6] = zipWith(Add, [1,2], [3,4,5]),
	ok.

% c(ziptest).
%
% ziptest:test().
