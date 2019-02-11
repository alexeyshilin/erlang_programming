-module(funcs).
-export([test/0]).
-vsv(0.1).

func_map(S, E, F)->
	Res = lists:seq(S, E),
	func_map(Res, F).

func_map([H|[]], F)->
	F(H);

func_map([H|T], F)->
	F(H),
	func_map(T, F).

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

	%Res = lists:seq(1, 10),
	%io:format("[~p]", [Res]),

FPrint = fun(V)->
	io:format("[~p]", [V])
end,

	func_map(1,10, FPrint),

	ok.


% c(funcs).
%
% funcs:test().
