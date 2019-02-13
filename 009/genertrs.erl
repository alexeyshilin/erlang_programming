-module(genertrs).
-export([test/0]).
-vsv(0.1).

intersection(List1, List2)->
	[ X || X <-List1, Y<-List2, X==Y].

difference(List1, List2)->
	Intr = intersection(List1, List2),
	(List1--Intr) ++ (List2--Intr).

test()->
	[3,6,9] = [X || X <- lists:seq(1,10), X rem 3 == 0],

	[1, 100, 9] = [ X || X <- [1, hello, 100, boo, "boo", 9], is_integer(X)],
	
	[4, 5] = [ X || X <-[1,2,3,4,5], Y<-[4,5,6,7,8], X==Y],

	[1,2,3,6,7,8] = difference([1,2,3,4,5], [4,5,6,7,8]),

	ok.

% c(genertrs).
%
%