-module(event_manager).
-export([start/2, stop/1]).
-export([add_handler/3, delete_handler/2, get_data/2, send_event/2]).
-export([init/1]).

start(Name, HandlerList) ->
	register(Name, spawn(event_manager, init, [HandlerList])), ok.

init(HandlerList) ->
	loop(initialize(HandlerList)).

initialize([]) -> [];

initialize([{Handler, InitData}|Rest]) ->
	[{Handler, Handler:init(InitData)}|initialize(Rest)].

stop(Name) ->
	Name ! {stop, self() } ,
	receive { reply, Reply} -> Reply end.

terminate([]) -> [ ] ;

terminate([{Handler, Data}|Rest]) ->
	[{Handler, Handler:terminate(Data)}|terminate(Rest)].

add_handler(Name, Handler, InitData) ->
	call(Name, {add_handler, Handler, InitData} ) .

delete_handler(Name, Handler) ->
	call(Name, { delete_handler, Handler}).

get_data(Name, Handler) ->
	call(Name, {get_data, Handler}).

send_event(Name, Event) ->
	call(Name, {send_event, Event}).

handle_msg({add_handler, Handler, InitData}, LoopData) ->
	{ok, [{Handler, Handler:init(InitData)}|LoopData]};

handle_msg({delete_handler, Handler}, LoopData) ->
	case lists:keysearch(Handler, 1, LoopData) of
		false ->
			{{error, instance}, LoopData};
		{value, {Handler, Data}} ->
			Reply = {data, Handler:terminate(Data)},
			NewLoopData = lists:keydelete(Handler, 1, LoopData),
			{Reply, NewLoopData}
	end;

handle_msg({get_data, Handler},LoopData) ->
	case lists:keysearch(Handler,1, LoopData) of
		false -> {{error, instance}, LoopData};
		{value, {Handler, Data}} -> {{data, Data}, LoopData}
	end;

handle_msg({send_event, Event}, LoopData) ->
	{ok, event(Event, LoopData)}.

event (Event, []) -> [];
event(Event, [{Handler, Data}|Rest]) ->
	[{Handler, Handler:handle_event(Event, Data)}|event(Event, Rest)].

call(Name, Msg) ->
	Name ! {request, self(), Msg},
	receive {reply, Reply} -> Reply end.

reply(To, Msg) ->
	To ! {reply, Msg}.

loop(State) ->
	receive
		{request, From, Msg} ->
			{Reply, NewState} = handle_msg(Msg, State),
			reply(From, Reply),
			loop(NewState);
		{stop, From} ->
			reply(From, terminate(State))
	end.


% c(event_manager).
% c(io_handler).
% c(log_handler).
%
% event_manager:start(alarm, [ { log_handler, "AlarmLog"}]).
% event_manager:send_event(alarm, {raise_alarm, 10, cabinet_open}).
% event_manager:add_handler(alarm, io_handler, 1).
% event_manager:send_event(alarm, {clear_alarm, 10, cabinet_open}).
% event_manager:send_event(alarm, {event, 156, linkup}).
% event_manager:get_data(alarm, io_handler).
% event_manager:delete_handler(alarm, stats_handler).
% event_manager:stop(alarm).



%
%17> event_manager:start(alarm, [ { log_handler, "AlarmLog"}]).
%ok
%18> event_manager:send_event(alarm, {raise_alarm, 10, cabinet_open}).
%ok
%19> event_manager:add_handler(alarm, io_handler, 1).
%ok
%20> event_manager:send_event(alarm, {clear_alarm, 10, cabinet_open}).
%#1,2019:01:13,23:39:03,clear,10,cabinet_open
%ok
%21> event_manager:send_event(alarm, {event, 156, linkup}).
%ok
%22> event_manager:get_data(alarm, io_handler).
%{data,2}
%23> event_manager:delete_handler(alarm, stats_handler).
%{error,instance}
%24> event_manager:stop(alarm).
%[{io_handler,{count,2}},{log_handler,ok}]
%25> 
%
