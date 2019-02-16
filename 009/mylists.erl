-module(mylists).
-export([test/0]).
-export([all/2]).
-vsn(-0).

	% all/2
	% any/2
% append/1
% append/2
% concat/1
% delete/2
% droplast/1
	% dropwhile/2
% duplicate/2
	% filter/2
% filtermap/2
% flatlength/1
% flatmap/2
% flatten/1
% flatten/2
% foldl/3
% foldr/3
% foreach/2
% join/2
% keydelete/3
% keyfind/3
% keymap/3
% keymember/3
% keymerge/3
% keyreplace/4
% keysearch/3
% keysort/2
% keystore/4
% keytake/3
% last/1
% map/2
% mapfoldl/3
% mapfoldr/3
% max/1
% member/2
% merge/1
% merge/2
% merge/3
% merge3/3
% min/1
% nth/2
% nthtail/2
	% partition/2
% prefix/2
% reverse/1
% reverse/2
	% search/2
% seq/2
% seq/3
% sort/1
% sort/2
% split/2
	% splitwith/2
% sublist/2
% sublist/3
% subtract/2
% suffix/2
% sum/1
	% takewhile/2
% ukeymerge/3
% ukeysort/2
% umerge/1
% umerge/2
% umerge/3
% umerge3/3
% unzip/1
% unzip3/1
% usort/1
% usort/2
% zip/2
% zip3/3
% zipwith/3
% zipwith3/4

%all
all(Pred, [])->
	false;

all(Pred, [H|[]])->
	Pred(H);

all(Pred, [H|T])->
	case Pred(H) of
		true -> all(Pred, T);
		false -> false
	end.
%any
any(Pred, [])->
	false;

any(Pred, [H|[]])->
	Pred(H);

any(Pred, [H|T])->
	case Pred(H) of
		true -> true;
		false -> any(Pred, T)
	end.

%dropwhile
dropwhile(Pred, [])->
	false;

dropwhile(Pred, [H|[]])->
	case Pred(H) of
		true -> [];
		false -> [H]
	end;

dropwhile(Pred, [H|T])->
	case Pred(H) of
		true -> [] ++ dropwhile(Pred, T);
		false -> [H] ++ dropwhile(Pred, T)
	end.

%filter
filter(Pred, [])->
	false;

filter(Pred, [H|[]])->
	case Pred(H) of
		true -> [H];
		false -> []
	end;

filter(Pred, [H|T])->
	case Pred(H) of
		true -> [H] ++ filter(Pred, T);
		false -> [] ++ filter(Pred, T)
	end.

%partition
partition(Pred, [])->
	{[],[]};

partition(Pred, [H|[]])->
	case Pred(H) of
		true -> {[H],[]};
		false -> {[],[H]}
	end;

partition(Pred, [H|T])->
	{A0,B0} = case Pred(H) of
		true -> {[H],[]};
		false -> {[],[H]}
	end,
	{A,B} = partition(Pred, T),
	{A0++A,B0++B}.

%search
search(Pred, [])->
	{};

search(Pred, [H|[]])->
	case Pred(H) of
		true -> {value, H};
		false -> {}
	end;

search(Pred, [H|T])->
	case Pred(H) of
		true -> {value, H};
		false -> search(Pred, T)
	end.

%splitwith
splitwith(Pred, [])->
	{[],[]};

splitwith(Pred, [H|[]])->
	case Pred(H) of
		true -> {[H],[]};
		false -> {[],[H]}
	end;

splitwith(Pred, [H|T])->
	{A0,B0} = case Pred(H) of
		true -> {[H],[]};
		false -> {[],[H]}
	end,
	{A,B} = if B0==[] -> splitwith(Pred, T);
				not(B0==[]) -> {A0,T}
			end,
	{A0++A,B0++B}.

%takewhile
takewhile(Pred, [])->
	[];

takewhile(Pred, [H|[]])->
	case Pred(H) of
		true -> [H];
		false -> []
	end;

takewhile(Pred, [H|T])->
	Res = case Pred(H) of
		true -> [H];
		false -> []
	end,
	Res2 = if not(Res==[]) -> takewhile(Pred, T);
				Res==[] -> []
			end,
	Res++Res2.

test()->
	
	F1 = fun(Val)->
		if
			Val =< 100 -> true;
			Val > 100 -> false
		end
	end,

	true = all(F1, [1,5,10,55]),
	false = all(F1, [1,5,10,55,300,15]),

	F2 = fun(Val)->
		if
			Val == 100 -> true;
			not(Val == 100) -> false
		end
	end,

	true = any(F2, [1,5,10,55,100]),
	false = any(F2, [1,5,10,55,300,15]),

	[105,300] = dropwhile(F1, [1,105,100,55,300,15]),

	[1,100,55,15] = filter(F1, [1,105,100,55,300,15]),

	{[1,100,55,15],[105,300]} = partition(F1, [1,105,100,55,300,15]),
	
	{} = search(F1, [1200,105,1001,550,300,150]),
	{value, 77} = search(F1, [1200,105,77,550,300,150]),
	
	F3 = fun(A) -> is_atom(A) end,
	{[a,b],[1,c,d,2,3,4,e]} = splitwith(F3, [a,b,1,c,d,2,3,4,e]),
	{[1],[2,3,4,5,6,7]} = splitwith(fun(A) -> A rem 2 == 1 end, [1,2,3,4,5,6,7]),

	Fbig = fun(C)->
		if C > 10 -> true;
			not(C > 10) -> false
		end
	end,

	[200,500,45] = takewhile(Fbig, [200,500,45,5,3,55,45,6]),
	Res1 = takewhile(Fbig, [200,500,45,5,3,55,45,6]),
	Res2 = lists:takewhile(Fbig,[200,500,45,5,3,55,45,6]),
	Res1 = Res2,

	ok.

% c(mylists).
%
% mylists:test().