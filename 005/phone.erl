-module(phone).
-export([start/0,stop/0]).
-export([incoming/1, off_hook/0 ,off_hook/1, on_hook/0, on_hook/1, other_on_hook/1, other_off_hook/1]).
-export([init/0]).
-export([test/0]).

% 5-5

start()->
    event_manager:start(billing, []),
    event_manager:add_handler(billing, log_handler, "billing.log"),
    %event_manager:add_handler(billing, io_handler, 1),

    Pid = spawn(phone, init, []),
    register(srv, Pid),
    
    io:fwrite("~p~n",[whereis(srv)]),

    ok.

stop()->
    event_manager:delete_handler(billing, log_handler),
    %event_manager:delete_handler(billing, io_handler),
    event_manager:stop(billing),

    unregister(srv),

    io:fwrite("~p~n",[whereis(srv)]),

    ok.

%%%%

init()->
    idle().

%%%%


incoming(Number)->
    srv ! {Number, incoming}.



off_hook()->
    srv ! off_hook.

off_hook(Number)->
    srv ! {Number, off_hook}.



on_hook()->
    srv ! on_hook.

on_hook(Number)->
    srv ! {Number, on_hook}.



other_on_hook(Number)->
    srv ! {Number, other_on_hook}.

other_off_hook(Number)->
    srv ! {Number, other_off_hook}.

%%%%

idle() ->
    receive
        {Number, incoming} ->
            start_ringing(),
            ringing(Number);
        off_hook ->
            start_tone(),
            dial()
    end.

ringing(Number) ->
    receive
        {Number, other_on_hook} ->
            stop_ringing(),
            idle();
        {Number, off_hook} ->
            stop_ringing(Number),
            start_chat(Number),
            connected(Number)
    end.


dial()->
    receive
        on_hook ->
            stop_tone(),
            idle();
        {Number, other_off_hook} ->
            start_chat(Number),
            connected(Number)
    end.

connected(Number)->
    receive
        {Number, on_hook} ->
            stop_chat(Number),
            stop_tone(),
            idle();
        {Number, other_on_hook} ->
            stop_chat(Number),
            stop_tone(),
            idle()
    end.





start_ringing()->
    ringing_start.

start_ringing(Number)->
    %event_manager:send_event(log, {incoming, {date(),time()}, Number}),
    ringing_start.

stop_ringing()->
    ringing_stop.

stop_ringing(Number)->
    %event_manager:send_event(log, {accept_incoming, {date(),time()}, Number}),
    ringing_stop.

start_tone()->
    tone_start.

stop_tone()->
    tone_stop.



start_chat(Number)->
    event_manager:send_event(billing, {chat_start, {date(),time()}, Number}),
    ok.

stop_chat(Number)->
    event_manager:send_event(billing, {chat_stop, {date(),time()}, Number}),
    ok.

%%%

test()->
    ok.


% c(phone).
%
% phone:start().
    % phone:on_hook().
    % phone:on_hook(111).
% phone:incoming(222).
    % phone:incoming(333).
% phone:off_hook(222).
% phone:on_hook(222).
%
    % phone:off_hook(333).
% phone:off_hook().
% phone:other_off_hook(333).
% phone:other_on_hook(333).
%
% phone:stop().
