-module(funcs).
-export([test/0]).
-vsv(0.1).



func_map([H|[]], V, F)->
	F(H,V);

func_map([H|T], V, F)->
	F(H, V),
	func_map(T, V, F);



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

FPrint2 = fun(V1, V2)->
	if V1 =< V2 ->
		io:format("[~p]", [V1]);
		V1 > V2 ->
			ok
	end
end,

	func_map(lists:seq(1, 10), 3, FPrint2),

	ok.


% c(funcs).
%
% funcs:test().
