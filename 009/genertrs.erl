-module(genertrs).
-export([test/0]).
-vsv(0.1).



test()->
	[3,6,9] = [X || X <- lists:seq(1,10), X rem 3 == 0],

	[1, 100, 9] = [ X || X <- [1, hello, 100, boo, "boo", 9], is_integer(X)],

	ok.

% c(genertrs).
%
%