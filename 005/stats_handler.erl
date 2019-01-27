-module(stats_handler).
-export( [init/1 , terminate/1, handle_event/2]).

% 5-4

init() -> dict:new().

init(Data) -> Data.

terminate(Data) ->
	{stat, Data}.

handle_event({Action, Id, Event}, Data) ->
	Key = {Action, Event},
	case dict:find(Key, Data) of
		error ->
			Count = 1,
			DataNew = dict:store(Key, Count, Data),
			DataNew;
		{ok, Count} ->
			DataNew = dict:store(Key, Count+1, Data),
			DataNew
	end;
	
handle_event(_,_) ->
	{error, stats_handler, handle_event, 2}.
