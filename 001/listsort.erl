-module(listsort).
-export([split/1, fast/1, merged/1, merge/2]).

%% fast

fast([])->
	[];

fast([Head|[]])->
	[Head];

fast([Head|Tail])->
	[A,B] = mycompare(Head,Tail,[],[]),
	fast(A)++[Head]++fast(B).


mycompare(Val, [], ValsL, ValG) ->
	[ValsL, ValG];

mycompare(Val, [Head|[]], ValsL, ValG) ->
	if
		(Head < Val) -> Res = [[Head|ValsL],ValG];
		((Head > Val) or (Head==Val)) -> Res =[ValsL,[Head|ValG]]
	end,
	Res;

mycompare(Val, [Head|Tail], [], [])->
	if
		(Head < Val) -> Res = mycompare(Val, Tail, [Head], []);
		((Head > Val) or (Head==Val)) -> Res = mycompare(Val, Tail, [], [Head])
	end,
	Res;

mycompare(Val, [Head|Tail], ValsL, ValG)->
	if
		(Head < Val) -> Res = mycompare(Val, Tail, [Head|ValsL], ValG);
		((Head > Val) or (Head==Val)) -> Res = mycompare(Val, Tail, ValsL, [Head|ValG])
	end,
	Res.

% listsort:fast([5,21,2,9,7,3,11]).

%% merged

merged(L)->
	merge_sort(L).


merge_sort_([H|T])->
	L = [H|T],

	{A,B} = split(L),

	if
		length(A) > 2 ->
			A1 = merge_sort(A),
			B1 = merge_sort(B),
			Res = merge(A1,B1);
		length(A) == 2 ->
			Res = merge(A,B);
		length(A) == 1 ->
			Res = merge(A)
	end,
	Res.

merge_sort([H|T])->
	L = [H|T],
	{A,B} = split(L),

	if
		length(A) > 1 ->
			merge(merge_sort(A), merge_sort(B));
		length(A) == 1 ->
			merge(A,B)
	end.


merge([H1|[]], []) ->
	[H1];

merge([], [H2|[]]) ->
	[H2];


merge([H1|[]], [H2|[]]) when H1<H2 ->
	[H1,H2];

merge([H1|[]], [H2|[]]) when H1>H2 ->
	[H2,H1];

merge([H1|[]], [H2|[]]) when H1==H2 ->
	[H1,H2];




merge([H1|[]], [H2|T2]) when H1<H2 ->
	% [[H1,H2] | T2];
	[H1,H2] ++ T2;

merge([H1|[]], [H2|T2]) when H1>H2 ->
	[H2|merge([H1],T2)];

merge([H1|[]], [H2|T2]) when H1==H2 ->
	% [[H1,H2] | T2];
	[H1,H2] ++ T2;



merge([H1|T1], [H2|[]]) when H1<H2 ->
	[H1|merge(T1,[H2])];

merge([H1|T1], [H2|[]]) when H1>H2 ->
	% [[H2,H1]|T1];
	[H2,H1] ++ T1;

merge([H1|T1], [H2|[]]) when H1==H2 ->
	% [[H1,H2] | T1];
	[H1,H2] ++ T1;



merge([H1|T1], [H2|T2]) when H1<H2 ->
	[H1 | merge(T1, [H2|T2])];

merge([H1|T1], [H2|T2]) when H1>H2 ->
	[H2 | merge([H1|T1], T2)];

merge([H1|T1], [H2|T2]) when H1==H2 ->
	[H1,H2] ++ merge(T1,T2);



merge(H1, [H2|T2]) when H1<H2 ->
	[H1,H2] ++ T2;

merge(H1, [H2|T2]) when H1>H2 ->
	[H2 | merge(H1, T2)];

merge(H1, [H2|T2]) when H1==H2 ->
	[H1,H2] ++ T2;



merge([H1,T1], H2) when H1<H2 ->
	[H1,H2] ++ T1;

merge([H1|T1], H2) when H1>H2 ->
	[H2,H1] ++ T1;

merge([H1|T1], H2) when H1==H2 ->
	[H1,H2] ++ T1;


merge(A, B) when A < B ->
	[A,B];
merge(A, B) when A > B ->
	[B,A];
merge(A, B) when A == B ->
	[A,B];

merge(_, _) ->
	error.

merge(A)->
	err.




dosort(A,B)->
	ok.

split(L)->
	split(L,[],[]).

split([H|[]],L1,L2)->
	if length(L1) == length(L2) ->
			{L1++[H],L2};
		not(length(L1) == length(L2)) ->
			{L1,L2++[H]}
	end;

split([H|T],L1,L2)->

	% case (length(T) > length(L1)) of
	case (length(T) > length(L1)) or (length(T) == length(L1)) of
	% case  ((length(L1) == 0) and (length(T) == 0)) or (length(T) > length(L1)) or (length(T) == length(L1)) of
			true -> B = 1;
			false -> B = 2
	end,

	if
		B==1 ->
			split(T, L1++[H], L2);
		B==2 ->
			split(T, L1, L2++[H])
	end.

% listsort:merged([5,21,2,9,7,3,11]).
% listsort:merged([5,21,2]).

% listsort:merge([1,7,9],[2,3,10]).
% listsort:merge([250,290,900,950,960,4000],[300,800,1000]).

% listsort:merged([250,290,900,950,960,4000,300,800,1000]).

% listsort:merged([9000,2900,2500,1000]).
% listsort:merged([9000,2900,2500,1000,1111,2222]).



% listsort:merge(listsort:merge([9000],[2900]), listsort:merge([2500],[1000])).

% listsort:merged([9000,2900,2500,1000,1500,1700]).
% listsort:merged([9000,2900,2500,1000,1500,1700,1122,2211]).

