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



func_map2([H|[]], F)->
	F(H);

func_map2([H|T], F)->
	F(H),
	func_map2(T, F).


func_map3([H|[]], F)->
	F(H);

func_map3([H|T], F)->
	F(H),
	func_map3(T, F).


func_transform([], F)->
	[];

func_transform([H|[]], F)->
	H;

func_transform([H|T], F)->
	func_transform([H|T], F, []).

func_transform([H|T], F, [])->
	Res = F(H, []),
	func_transform(T, F, Res);

func_transform([], F, Accum)->
	Accum;

func_transform([H|[]], F, Accum)->
	Res = F(H, Accum),
	Res;

func_transform([H|T], F, Accum)->
	Res = F(H, Accum),
	func_transform(T, F, Res).


test()->

Fun1 = fun(S, E, Func) ->
	io:format("[~p]", [S]),
	if
		S < E -> Func(S+1, E, Func);
		S == E -> ok;
		S > E -> ok
	end
end,


	%Fun1(1, 10, Fun1),

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

	%func_map(lists:seq(1, 10), 3, FPrint2),

FPrintOddEven = fun(V)->
	if V rem 2 == 0 ->
		io:format("[~p]", [V]);
		not(V rem 2 == 0) ->
			ok
	end
end,

	%func_map2(lists:seq(1, 10), FPrintOddEven),

FuncJoin = fun(V, Accum)->
	Accum ++ V
end,

	LList = [[1],[1,2,9],[4,1,0,7],[9,2]],

	Res = func_transform(LList, FuncJoin),
	Res = [1,1,2,9,4,1,0,7,9,2],

FuncSum = fun(V, Sum)->
	if Sum == [] -> 0+V;
		not(Sum == []) -> Sum + V
	end
end,

	Res2 = func_transform(Res, FuncSum),
	Res2 = 1+1+2+9+4+1+0+7+9+2,

	ok.


% c(funcs).
%
% funcs:test().
