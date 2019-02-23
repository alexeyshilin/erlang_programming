-module(indexer).
-export([test/0]).
-export([indexer/0]).
-include("indexer.hrl").
-vsn(-0).

-define(Punctuation,"[ |,|\\.|;|:|\\t|\\n|\\(|\\)]+").

index(File) ->
	Id = ets:new(indexTable, [ordered_set, named_table]),
	processFile(File),
	prettyIndex(),
	Id.

close(Id)->
	ets:delete(Id).

processFile(File) ->
	{ok, IoDevice} = file:open(File, [read]),
	processLines(IoDevice,1).

processLines(IoDevice,N) ->
	case io:get_line(IoDevice,"") of
		eof ->
			ok;
		Line ->
			processLine(Line,N),
			processLines(IoDevice,N+1)
	end.

processLine(Line,N) ->
	Words = re:split( Line, ?Punctuation,[{return, list}] ),
	processWords(Words,N).

processWords(Words,N) ->
	case Words of
		[ ] -> ok;
		[Word|Rest] ->
			if
				length(Word) > 3 ->
					Normalise = string:to_lower(Word),
					ets:insert(indexTable,{{Normalise , N } } ) ;
				true -> ok
			end,
		processWords(Rest,N)
	end.


prettyIndex() ->
	case ets:first( indexTable ) of
		'$end_of_table' ->
			ok;
		First ->
			case First of
				{Word, N} ->
					IndexEntry = {Word, [N]}
			end,
			prettyIndexNext(First,IndexEntry)
	end.

prettyIndexNext(Entry,{Word, Lines}=IndexEntry) ->
	Next = ets:next(indexTable,Entry),
	case Next of
		'$end_of_table' ->
			prettyEntry(IndexEntry);
		{NextWord, M} ->
			if
				NextWord == Word ->
					prettyIndexNext(Next,{Word, [M|Lines]});
				true ->
					prettyEntry(IndexEntry),
					prettyIndexNext(Next,{NextWord, [M]})
			end
	end.

prettyEntry({Word, Lines}=IndexEntry)->
	% accumulate/1
	% prettyList/1
	% pad/2
	Lines2 = prettyList(accumulate(Lines)),
	io:format("~s ~p \n", [pad(15, Word), Lines2]),

	ok.


%accumulate
accumulate([])->
	[];

accumulate([H|[]])->
	nice_range(index_line2range([H|[]]));

accumulate([H|T])->
	nice_range(index_line2range([H|T])).
%/accumulate

%nice_range
nice_range([])->
	[];

nice_range([{A,B}|[]]) when A==B->
	[{A}];

nice_range([{A,B}|T]) when A==B->
	[{A}]++nice_range(T);

nice_range([{A,B}|[]])->
	[{A,B}];

nice_range([{A,B}|T])->
	[{A,B}]++nice_range(T).

%/nice_range

%prettyList
prettyList([])->
	[];

prettyList([{A}|[]])->
	[48+A];

prettyList([{A}|T])->
	[48+A]++"," ++ prettyList(T);

prettyList([{A,B}|[]])->
	[48+A]++"-"++[48+B];

prettyList([{A,B}|T])->
	[48+A]++"-"++[48+B]++"," ++ prettyList(T);

prettyList(Any)->
	error.
%/prettyList

%repeat
repeat(N, []) ->
	[];

repeat(N, [H|[]]) when N<0 ->
	error;

repeat(N, [H|[]]) when N==0 ->
	[];

repeat(N, [H|[]]) when N>0 ->
	[H] ++ repeat(N-1, [H]);

repeat(N, [H|T]) when N==0 ->
	error;

repeat(N, [H|T]) ->
	error;

repeat(N, S) ->
	%[S] ++ repeat(N-1, S).
	unknown_error;

repeat(N, S) when N==0 ->
	[];

repeat(N, S) when N>0 ->
	%[S] ++ repeat(N-1, S).
	unknown_error2.

%pad
pad(N,[])->
	repeat(N, " ");

pad(N,[H|[]])->
	[H]++repeat(N-1, " ");

pad(N,[H|T])->
	[H|T]++repeat(N-length([H|T]), " ").
%/pad

%%fast
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
%/fast

%index_line2range
index_line2range(IndexIn)->
        Index = fast(IndexIn),
        index_line2range(Index, [], 0, 0).

index_line2range([H|[]], Accum, CurStart, Curr) when (Curr==H) or ( (Curr+1) == H) ->
        Accum ++ [{CurStart,H}];

index_line2range([H|[]], Accum, CurStart, Curr) ->
        Accum ++ [{CurStart,Curr}, {H,H}];

index_line2range([H|T], Accum, CurStart, Curr) when (CurStart==0) and (Curr==0) ->
        index_line2range(T, Accum, H, H);


index_line2range([H|T], Accum, CurStart, Curr) when (Curr==H) or ( (Curr+1) == H) ->
        index_line2range(T, Accum, CurStart, H);

index_line2range([H|T], Accum, CurStart, Curr) ->
        index_line2range(T, Accum ++ [{CurStart,Curr}], H, H);

index_line2range(_,_,_,_)->
        err.
%/index_line2range



test()->
	"sss" = repeat(3, "s"),
	"test      " = pad(10, "test"),
	[{1},{3},{5,7}] = accumulate([7,6,6,5,3,3,1,1]),
	"1,3,5-7" = prettyList([{1}, {3} ,{5,7}]),
	
	Id = index("indexer.txt"),
	close(Id),

	ok.

indexer()->
	ok.

% c(indexer).
%
% indexer:test().
% tv: start ()
