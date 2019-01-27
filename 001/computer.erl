-module(computer).
-export([calculator/1, printer/1, compiler/1, simulator/1, converter/1]).
-export([code_printer/1, whatisit/1, to_number/1, get_number/1, convert_to_number/1, parser/1]).


% 3-8

% ((2+3)-4)		4	~((2*3)+(3*4))
% if ((2+3)-4) then 4 else ~((2*3)+(3*4))
% let с = ((2+3)-4) in ~((2*с)+(3*4))

% {minus, {plus, {num, 2}, {num, 3}}, {num, 4}}

calculator()->
	err.

printer()->
	err.

compiler()->
	err.

simulator()->
	err.

converter()->
	err.

%% -----------------------------

calculator_([H|T])->
	% io:format("~p~n", T);
	% io:format("~c", [H]);
	% io:format("~B", [H]),
	
	if
		H==40 ->
			io:format("*(*");
		H==41 ->
			io:format("*)*");
		H==49 ->
			io:format("-1-");
		H==50 ->
			io:format("-2-");
		H==51 ->
			io:format("-3-")
	end.



calculator({num,Val}) ->
	Val;

calculator({Oper,Oper1,Oper2}) ->

	Res = if
				Oper == sum -> calculator(Oper1) + calculator(Oper2);
				Oper == dif -> calculator(Oper1) - calculator(Oper2);
				Oper == mult -> calculator(Oper1) * calculator(Oper2);
				Oper == dvd -> calculator(Oper1) / calculator(Oper2);
				not(true==false) -> operation_unknown
			end,
	Res;

calculator(E)->
	err.

%% -----------------------------

compiler(E)->
	compiler(E, []);

compiler(_)->
	err.


compiler({Oper, {num,Val1}, {num,Val2}}, L)->
	L ++ [Val1, Val2, Oper];

compiler({Oper, {num,Val1}, Oper2}, L)->
	Res1 = compiler(Oper2, L),
	Res1 ++ [Val1, Oper];

compiler({Oper, Oper1, {num,Val2}}, L)->
	Res1 = compiler(Oper1, L),
	Res1 ++ [Val2, Oper];

compiler({Oper, Oper1, Oper2}, L)->
	Res1 = compiler(Oper1, L),
	Res2 = compiler(Oper2, Res1),
	Res2 ++ [Oper];

compiler(A,L)->
	{err, A, L}.


%% -----------------------------


simulator([A|[B|[C|[]]]]) when (C==sum) or (C==dif) or (C==mult) or (C==dvd)->
	Oper = C,
	Oper1 = A,
	Oper2 = B,

	Res = if
				Oper == sum -> Oper1 + Oper2;
				Oper == dif -> Oper1 - Oper2;
				Oper == mult -> Oper1 * Oper2;
				Oper == dvd -> Oper1 / Oper2;
				not(true==false) -> operation_unknown
			end,
	Res,

	[Res];

simulator([A|[B|[C|T]]]) when (C==sum) or (C==dif) or (C==mult) or (C==dvd)->
	Oper = C,
	Oper1 = A,
	Oper2 = B,

	Res = if
				Oper == sum -> Oper1 + Oper2;
				Oper == dif -> Oper1 - Oper2;
				Oper == mult -> Oper1 * Oper2;
				Oper == dvd -> Oper1 / Oper2;
				not(true==false) -> operation_unknown
			end,
	Res,

	Res2 = simulator([Res] ++ T),
	Res2;


simulator([])->
	sim_end;

simulator([H|[]])->
	%{err_head, H};
	[H];

simulator([A|[B|[]]])->
	[A,B];

simulator([H|T])->
	%simulator([H]++simulator(T));
	%simulator(T);
	%Res = simulator([H]++simulator(T)),
	%Res;
	%Res = simulator([H]++simulator(T)),
	Res = [H]++simulator(T),
	simulator(Res);

simulator(E)->
	{err,E}.

%% -----------------------------

converter(E)->
	err.

%% -----------------------------

printer({num, Val})->
	% Val+48;
	% Val;
	%io:format("~B", [Val]);
	[Val+48];

printer({Oper, Operand1, Operand2})->
	
	Opersumb = if
				Oper == sum -> "+";
				Oper == dif -> "-";
				Oper == mult -> "*";
				Oper == dvd -> "/";
				not(true==false) -> operation_unknown
			end,

	Opersumb2= if
				Oper == sum -> 43;
				Oper == dif -> 45;
				Oper == mult -> 42;
				Oper == dvd -> 47;
				not(true==false) -> operation_unknown
			end,

	% io:format("(~B~s~B)", [printer(Operand1), Opersumb, printer(Operand2)]);
	% io:format("(~s~s~s)", [printer(Operand1), Opersumb, printer(Operand2)]);
	%Res = io_lib:format("(~s~s~s)", [printer(Operand1), Opersumb, printer(Operand2)]),
	%Res = io_lib:format("(~B)",[1]),
	% Res = ["("] ++ printer(Operand1) ++ [Opersumb] ++ printer(Operand2) ++ [")"],
	Res = [40] ++ printer(Operand1) ++ [Opersumb2] ++ printer(Operand2) ++ [41],
	% io:format("~p", Res);
	Res;


printer(_)->
	err.

%% -----------------------------

parser_([H|[]], [])->
	err;

parser_([H|T], [])->
	err;

parser_([H|[]], [Hn|[]])->
	err;

parser_([H|T], [Hn|[]])->
	err;


parser_([H|T], [Hn|Tn])->
	err;

parser_([H|[]], [Hn|Tn])->
	err;

parser_(_,_)->
	err.


%parser([H|T], [], {}) when H==40 -> % (
%	parser(T,[],{});

%parser([H|T], L, {}) when H==40 -> % (
%	parser(T,L,{});

%parser([H|T], L, N) when H==40 -> % (
%	parser(T,L,{});

parser([H|[]], L, N) when H==41 -> % )
	N;

parser([H|T], L, N) when H==41 -> % )
	parser(T,L,N);

parser([H|T], L, {Oper1}) when (H==43) or (H==45) or (H==42) or (H==47) -> % +-*/
	
	Oper =  if
		H == 43 -> sum;
		H == 45 -> dif;
		H == 42 -> mult;
		H == 47 -> dvd;
		not(true==false) -> err
	end,

	parser(T, [], {Oper,Oper1});

parser([H|T], L, N) when (H==43) or (H==45) or (H==42) or (H==47) -> % +-*/
	Oper =  if
		H == 43 -> sum;
		H == 45 -> dif;
		H == 42 -> mult;
		H == 47 -> dvd;
		not(true==false) -> err
	end,

	parser(T, [], {Oper,N});

parser([H|T], L, {}) when (H >= 48) and (H =< 57)->
	Oper1 = {num, H-48},
	parser(T,L,{Oper1});

parser([H|T], L, {Oper,Oper1}) when (H >= 48) and (H =< 57)->
	Oper2 = {num, H-48},
	parser(T,L,{Oper,Oper1,Oper2});


parser([H|T], L, {}) when H == 40 -> % (
	% {parser(T,L,{})};
	parser(T,L,{});

parser([H|T], L, {Oper,Oper1}) when H == 40 -> % (
	{Oper,Oper1,parser(T,L,{})};



parser(_,_,_)->
	err.

parser(L)->
	parser(L,[],{}).


%% -----------------------------

exp_start([H|T])->
err.

exp_end([H|T])->
err.

exp_operand([H|T])->
err.

exp_operation([H|T])->
err.

%% -----------------------------

integerpow(N, 1) -> N;
integerpow(N, M) -> N*integerpow(N, M-1).

%% -----------------------------

convert_to_number([H|T])->
	{Num,Ost} = get_number([H|T]),
	to_number(Num).

%% -----------------------------

to_number([H|[]])->
	to_number(H);

to_number([H|T])->
	% to_number(H)*math:pow(10,length(T)) + to_number(T);
	to_number(H)*integerpow(10,length(T)) + to_number(T);

to_number(Val) when (Val >= 48) and (Val =< 57)->
	Val-48;

to_number(Val)->
	% [Code] = io:format("~w", [Val]),

	if
		(Val >= 48) and (Val =< 57) ->
			Val - 48;
		not(true == false)->
			not_a_number
	end;

to_number(Val) ->
	not_a_number.

%% -----------------------------

get_number([H|T])->
	get_number([H]++T,[]).

get_number([],N)->
	{N,[]};

get_number([H|T],[]) when (H >= 48) and (H =< 57)->
	get_number(T, [H]);

get_number([H|T],N) when (H >= 48) and (H =< 57)->
	get_number(T, N++[H]);

get_number([H|T],N)->
	{N,[H]++T}.

%% -----------------------------

is_operations(Val)->
	lists:member(Val, [45,43,42,47]).

is_numbers(Val)->
	lists:member(Val, [49,50,51,52,53,54,55,56,57,48]).

is_groups(Val)->
	lists:member(Val, [40,41]).

is_spaces(Val)->
	lists:member(Val, [32]).

is_chars(Val)->
	(((Val >= 65) and (Val =< 90)) or ((Val >= 97) and (Val =< 122))).


whatisit([H | []])->
	whatisit(H);

whatisit([H | T])->
	whatisit(H);

whatisit(Var)->
	Oper = is_operations(Var),
	Num = is_numbers(Var),
	Group = is_groups(Var),
	Space = is_spaces(Var),
	Char = is_chars(Var),

	if
		Oper == true ->
			Res = oper;
		Num == true ->
			Res = num;
		Group == true ->
			Res = grp;
		Space == true ->
			Res = space;
		Char == true ->
			Res = char;
		not(Char == true) ->
			Res = unknown
	end,
	Res;

whatisit(_)->
	error.

%% -----------------------------


%% -----------------------------

code_printer([H|[]])->
	code_printer(H);

code_printer([H|T])->
	code_printer(H),
	code_printer(T);

code_printer(V)->
	io:format("[~B]", [V]).



% computer:calculator( "((2+3)-4)" ).

% computer:code_printer( "1234567890" ).
% [49][50][51][52][53][54][55][56][57][48]

% computer:code_printer( "-+*/()" ).
% [45][43][42][47][40][41]

% computer:code_printer( " .," ).
% [32][46][44]

% computer:printer( {dvd,{dif,{num,7},{num,3}},{mult,{num,2},{dif,{num,2},{num,1}}}} ).
% computer:printer( {num,7} ).
% computer:printer( {dif,{num,7},{num,3}} ).

% !!!
% computer:printer( computer:parser("((7-3)/(2*2))") ).


% computer:calculator( {dif,{num,7},{num,3}} ).
% computer:calculator( {dvd,{dif,{num,7},{num,3}},{mult,{num,2},{num,2}}} ).
% computer:calculator( computer:parser("((7-3)/(2*2))") ).

% computer:compiler( computer:parser("(7-3)") ).
% computer:compiler( computer:parser("((7-3)/(2*2))") ).
% computer:compiler( computer:parser("((7-3)/((2*2)-3))") ).

% computer:simulator([7,3,dif,2,2,mult,dif]).
% computer:simulator([7,3,dif,2,2,mult,3,dif,dvd]).

% computer:simulator([700,300,dif,200,200,mult,sum]).
