-module(genertrs).
-export([test/0]).
-vsv(0.1).


test()->
	[3,6,9] = [X || X <- lists:seq(1,10), X rem 3 == 0],
	ok.

% c(genertrs).
%
%