-module(index).
-export([test/0, get_file_data/1, create_index/1, print_index/2]).

% 3-9

get_file_data(Filename)->
	{ok, Device} = file:open(Filename, [read]),

	Lines = try
		get_lines(Device)
		after file:close(Device)
	end,

	Lines.

create_index([H|[]])->
	create_index([H|[]], 0, dict:new());

create_index([H|T])->
	create_index([H|T], 0, dict:new());

create_index(_)->
	{err, create_index, 1}.



create_index([H|[]], I, Index)->
	IndexNew = create_line_index(H, I+1, Index),
	IndexNew;

create_index([H|T], I, Index)->
	IndexNew = create_line_index(H, I+1, Index),
	IndexNew2 = create_index(T, I+1, IndexNew),
	%IndexNew2 = IndexNew,
	IndexNew2;

create_index(A,B,C)->
	{err, create_index, 3, {A,B,C}}.

create_index_(_,_,_)->
	{err, create_index, 3}.


create_line_index([], I, Index)->
	Index;

create_line_index([H|T], I, Index)->
	%Data = dict:fetch(H, Index),

	Data = case  dict:find(H, Index) of
		error -> [];
		{ok, Val} -> Val
	end,

	IndexOut = dict:store(H, Data ++ [I], Index),
	IndexOut2 = create_line_index(T,I,IndexOut),
	IndexOut2.

sort_line_index(Index)->
	Index. % todo: sort list of indexies

index_line2range(IndexIn)->
	Index = sort_line_index(IndexIn),
	index_line2range(Index, [], 0, 0).

index_line2range([H|[]], Accum, CurStart, Curr) ->
	Accum ++ [{CurStart,H}];

index_line2range([H|T], Accum, CurStart, Curr) when (CurStart==0) and (Curr==0) ->
	index_line2range(T, Accum, H, H);


index_line2range([H|T], Accum, CurStart, Curr) when (Curr==H) or ( (Curr+1) == H) ->
	index_line2range(T, Accum, CurStart, H);

index_line2range([H|T], Accum, CurStart, Curr) ->
	index_line2range(T, Accum ++ [{CurStart,Curr}], H, H);

index_line2range(_,_,_,_)->
	err.

print_index(Index, Key)->
	Data = case  dict:find(Key, Index) of
		error -> [];
		{ok, Val} -> Val
	end,
	index_line2range(Data).


get_lines(Device)->
	case io:get_line(Device, "") of
		eof  -> [];
		% Line -> Line ++ get_lines(Device)
		Line -> [words_list(Line)] ++ get_lines(Device)
	end.

words_list(Data)->
	words_list(Data, [], []).

words_list([H|[]], L, Accum) when (H==10) or (H==13)->
	L++[Accum];

words_list([H|[]], L, Accum)->
	L++[Accum++[H]];
	
words_list([H|T], L, Accum) when H==32 ->
	words_list(T, L++[Accum], []);

words_list([H|T], L, Accum) when (H==10) or (H==13)->
	words_list(T, L, Accum);

words_list([H|T], L, Accum) ->
	words_list(T, L, Accum ++ [H]);

words_list(_,_,_) ->
	error.

test()->
	ok.


% index:create_index( index:get_file_data("index.txt") ).
% index:print_index( index:create_index( index:get_file_data("index.txt") ), "Erlang" ).

