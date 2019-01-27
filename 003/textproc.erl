-module(textproc).
-export([test/0, get_file_data/1, text_limits/2, text_justify/2]).
-export([n_sumb/2]).

% 3-10

-define(Space, 32).
-define(Dlmt, "\n").

%-define(Space, "_").
%-define(Dlmt, "#").

get_file_data(Filename)->
	{ok, Device} = file:open(Filename, [read]),

	Lines = try
		get_lines(Device)
		after file:close(Device)
	end,

	Lines.

get_lines(Device)->
	case io:get_line(Device, "") of
		eof  -> [];
		%Line -> Line ++ get_lines(Device)
		Line -> string:trim(Line) ++ [32] ++ get_lines(Device)
	end.


text_limits(Text, Limit)->
	% Text2 = string:replace(Text, "\n", ""),
	text_limits(Text, Limit, 0, [], []).

text_limits([H|[]], Limit, Cur, Temp, Res)->

	Len = length(Res),

	Last = if Len > 0 -> lists:nth(Len, Res);
			Len =< 0 -> 0
			end,

	if ( Last == ?Dlmt) -> 
			Res++Temp++[H];
		not(true==false)->
			Res++[?Space]++Temp++[H]
	end;



text_limits([H|T], Limit, Cur, Temp, Res) when (Cur<Limit) and (H==32)->
	%io:fwrite("[~w]", [Cur]),
	%text_limits(T, Limit, Cur+1, [], Res++[32]++Temp);
	%text_limits(T, Limit, Cur+1, [], Res++[]++Temp);
	
	Len = length(Res),

	Last = if Len > 0 -> lists:nth(Len, Res);
			Len =< 0 -> 0
			end,

	if ( Last == ?Dlmt) -> 
			% text_limits(T, Limit, Cur+1, [], Res++[]++Temp);
			text_limits(T, Limit, Cur+1, [], Res++Temp);
		Res == [] -> 
			text_limits(T, Limit, Cur+1, [], Res++Temp);
		not(true==false) -> 
			text_limits(T, Limit, Cur+1, [], Res++[?Space]++Temp)
	end;


text_limits([H|T], Limit, Cur, Temp, Res) when (Cur<Limit)->
	%io:fwrite("[~w]", [Cur]),
	text_limits(T, Limit, Cur+1, Temp ++ [H], Res);



text_limits([H|T], Limit, Cur, Temp, Res) when (Cur==Limit) and (H==32)->
	text_limits(T, Limit, 0, [], Res ++ [?Space] ++Temp++[?Dlmt]); % "\n"

text_limits([H|T], Limit, Cur, Temp, Res) when (Cur==Limit)->
	%io:fwrite("[~w]", [Cur]),

	Len = length(Res),

	Last = if Len > 0 -> lists:nth(Len, Res);
			Len =< 0 -> 0
			end,

	% text_limits(T, Limit, length(Temp)+1, Temp ++ [H], Res++[?Dlmt]);

	if ( Last == ?Dlmt) -> 
			text_limits(T, Limit, length(Temp)+1, Temp ++ [H], Res); % "\n"
		not(Last == ?Dlmt) -> 
			text_limits(T, Limit, length(Temp)+1, Temp ++ [H], Res++[?Dlmt]) % "\n"
	end;



text_limits([H|T], Limit, Cur, Temp, Res) when (Cur>Limit) and (H==32)->
	Len = length(Res),

	Last = if Len > 0 -> lists:nth(Len, Res);
			Len =< 0 -> 0
			end,

	% text_limits(T, Limit, 0, [], Res ++ [?Space] ++Temp++[?Dlmt]); % "\n"

	if ( Last == ?Dlmt) -> 
			text_limits(T, Limit, 0, [], Res ++ Temp++[?Dlmt]); % "\n"
		not(Last == ?Dlmt) -> 
			text_limits(T, Limit, 0, [], Res ++ [?Space] ++Temp++[?Dlmt]) % "\n"
	end;

%text_limits([H|[]], Limit, Cur, Temp, Res) when (Cur>Limit)->
%	io:fwrite("*"),io:fwrite(Res++Temp++[H]),io:fwrite("*");

text_limits([H|T], Limit, Cur, Temp, Res) when (Cur>Limit)->
	%io:fwrite("*"),io:fwrite(Temp),io:fwrite("*"),
	text_limits(T, Limit, length(Temp)+1, Temp ++ [H], Res); % "\n"



%text_limits([H|T], Limit, Cur, Temp, Res) when (Cur==Limit)->
%	%text_limits(T, Limit, length(Temp)+1, Temp ++ [H], Res++[10]); % "\n"
%	text_limits(T, Limit, 0, Temp ++ [H], Res); % "\n"

text_limits(A,B,C,D,E)->
	{err, text_limits, {A,B,C,D,E}};

text_limits(_,_,_,_,_)->
	err.

text_justify(Text, Limit)->
	Text2 = text_limits(Text, Limit),
	Text3 = text_justify_(Text2, Limit);

text_justify(_,_)->
	{error, text_justify, 2}.





text_justify_(Text, Limit)->
	text_justify_(Text, Limit, [], 0, []);

text_justify_(_,_)->
	{error, text_justify_, 2}.


text_justify_([H|[]], Limit, Line, Spaces, Accum)->
	[H];
	%err;

text_justify_([H|T], Limit, Line, Spaces, Accum) when (H==?Space)->
	text_justify_(T, Limit, Line++[H], Spaces+1, Accum);

text_justify_([H|T], Limit, Line, Spaces, Accum) when (H==?Dlmt)-> %line
	Res = Accum ++ text_justify_line(Line, Limit, Spaces) ++ [?Dlmt],
	Res2 = text_justify_(T, Limit, [], 0, []),
	%[Res]++[Res2];
	[Res]++[Res2];

text_justify_([H|T], Limit, Line, Spaces, Accum)->
	text_justify_(T, Limit, Line++[H], Spaces, Accum);

text_justify_(A,B,C,D,E)->
	%error;
	{error, text_justify_, {a,A,b,B,c,C,d,D,e,E}};

text_justify_(_,_,_,_,_)->
	error.


text_justify_line(Line, Limit, Spaces)->

	%N = math:floor( (Limit - length(Line))/Spaces ),
	%N = trunc( (Limit - length(Line))/Spaces ),
	
	N = if Spaces=<0 -> 0;
		Spaces>0 -> trunc( (Limit - length(Line))/Spaces )
	end,

	%io:fwrite("[~w]", [N]).
	text_justify_line_(Line, length(Line), Limit, Spaces, N, 0, []).
	


text_justify_line_([H|[]], Len, Limit, Spaces, N, SpacesAdded, Accum)->
	Accum++[H];

text_justify_line_([H|T], Len, Limit, Spaces, N, SpacesAdded, Accum) when H==?Space ->
	
	ToAdd = Limit-Len-SpacesAdded,

	N2 = if 
			%(ToAdd > N) and (N<0) -> ToAdd+5
			(N=<0) -> ToAdd;
			ToAdd >= N -> N
		end,

	text_justify_line_(T, Len, Limit, Spaces, N, SpacesAdded+N2, Accum++[H]++n_sumb(" ", N2));

text_justify_line_([H|T], Len, Limit, Spaces, N, SpacesAdded, Accum)->
	text_justify_line_(T, Len, Limit, Spaces, N, SpacesAdded, Accum++[H]);

text_justify_line_(A,B,C,D,E,F,G)->
	{error, text_justify_line_, {A,B,C,D,E,F,G}}.




n_sumb(S, 0)->
	[];

n_sumb(S, N) when N<0 ->
	%{error, n_sumb, "N<0"};
	[];

n_sumb(S, N)->
	[S]++n_sumb(S, N-1).

get_line_stat([H|T])->
	err.


test()->
	% io:format("~s", textproc:text_limits(textproc:get_file_data("textproc.txt"), 30)).
	%io:format( textproc:text_limits(textproc:get_file_data("textproc.txt"), 30), []).

	io:format( textproc:text_justify(textproc:get_file_data("textproc.txt"), 30), []).
	


% textproc:text_limits(textproc:get_file_data("textproc.txt"), 30).
% io:format( textproc:text_limits(textproc:get_file_data("textproc.txt"), 30), []).
% textproc:test().

% textproc:n_sumb("_", 5).

