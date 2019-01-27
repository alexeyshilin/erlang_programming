-module(fsm).
-export([idle/0,ringing/1]).

idle() ->
    receive
        {Number, incoming} ->
            start_ringing(),
            ringing(Number);
        offhook ->
            start_tone(),
            dial()
    end.

ringing(Number) ->
    receive
        {Number, other_on_hook} ->
            stop_ringing(),
            idleO;
        {Number, off_hook} ->
            stop_ringing(),
            connected(Number)
    end.


start_ringing()->
    ok.

start_tone()->
    ok.

stop_ringing()->
    ok.

dial()->
    ok.

connected(Number)->
    ok.
