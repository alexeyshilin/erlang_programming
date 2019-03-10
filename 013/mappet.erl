-module(mappet).
-export([test/0]).
-export([salary_up/1]).
-include("mappet.hrl").

salary_up(Perc) ->
	F = fun() ->
		FoldFun = fun(#muppet{name=Name, callsign=Call, salary = Salary}, _) ->
			NewSalary = Salary + Salary/Perc,
			Rec = #muppet{name=Name, callsign=Call, salary = NewSalary },
			mnesia:write(Rec),
			ok
		end,
	mnesia:foldl(FoldFun, ok, muppet)
	end,
	{atomic, ok} = mnesia:transaction(F),
	ok.

test()->
	mappet:salary_up(10).

% c(mappet).
%
% mappet:test().
