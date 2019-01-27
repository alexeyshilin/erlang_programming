-module(mutex).
-export([start/0, stop/0]).
-export([wait/0, signal/0]).
-export([init/0]).

% Server
start() ->
    register(mutex, spawn(?MODULE, init, [])).

stop() ->
    mutex ! stop.

wait() ->
    mutex ! {wait, self()},
    receive ok -> ok end.

signal() ->
    mutex ! {signal, self()}, ok.

% /Server


% Client

init() ->
    free().

free() ->
    receive
        {wait, Pid} ->
            Pid ! ok,
            busy(Pid);
        stop ->
            terminate()
    end.

busy(Pid) ->
    receive
        {signal, Pid} ->
        free()
    end.


terminate() ->
    receive
        {wait, Pid} ->
            exit(Pid, kill),
            terminate()
    after
        0 -> ok
    end.

% /Client

