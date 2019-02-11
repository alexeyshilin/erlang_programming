-module(funcs).
-export([test/0]).
-vsv(0.1).


test()->

Fun1 = fun(S, E, Func) ->
	io:format("[~p]", [S]),
	if
		S < E -> Func(S+1, E, Func);
		S == E -> ok;
		S > E -> ok
	end
end,


	Fun1(1, 10, Fun1),

	ok.


% c(funcs).
%
% funcs:test().
