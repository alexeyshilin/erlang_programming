-module(mylists).
% -export([filter/2, reverse/1, concatenate/1, flatten/1]).
-export([filter/2, reverse/1, concatenate/1, flatten/1]).

% 3-5

% filter( [ 1,2,3,4,5 ] , 3 ) => [ 1,2,3 ] .
% reverse( [ 1,2,3 ] ) => [ 3,2,1 ] .
% concatenate([[1,2,3], [], [4, five]]) => [1,2,3,4,five].
% flatten( [ [ 1,[2,[3] , [] ] ] , [ [ [4] ] ], [5,6] ] ) => [ 1,2,3,4,5,6 ] .



% filter
filter(SomeList, Val) ->
	filter(SomeList, Val, []).

filter([], Val, Accum) ->
	Accum;

filter([Head | Tail], Val, Accum) ->
	if
		((Head < Val) or (Head==Val))  -> filter(Tail, Val, Accum++[Head]);
		(Head > Val) -> filter(Tail, Val, Accum)
	end.


% reverse
reverse([]) ->
	[];

reverse([Head | []]) ->
	[Head];

reverse([Head | Tail]) ->
	reverse(Tail) ++ [Head].



% concatenate
concatenate([])->
	[];

concatenate(Val)->
	concatenate2(Val).

concatenate2([Head | []])->
	Head;

concatenate2([Head | Tail])->
	Head ++ concatenate2(Tail);

concatenate2(Val)->
	Val.

% concatenate([[l,2,3], [], [4, five]]) => [1,2,3,4,five].
% mylists:concatenate([[1,2,3], [], [4, five]]).
% mylists:concatenate( [[1,[2,[3],[]]],[[[4]]],[5,6]] ).

% flatten
flatten([])->
	[];

flatten([Head | []])->
	flatten(Head);

flatten([Head | Tail])->
	flatten(Head) ++ flatten(Tail);

flatten(Val)->
	[Val].

% mylists:flatten( [[1,[2,[3],[]]],[[[4]]],[5,6]] ).
