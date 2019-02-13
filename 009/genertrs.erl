-module(genertrs).
-export([test/0]).
-vsv(0.1).


test()->
	Res = [X || X <- lists:seq(1,10), X rem 3 == 0],
	Res.

% c(genertrs).
%
%