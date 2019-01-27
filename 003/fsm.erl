-module(fsm).
-export([idle/0,ringing/1]).

idle() ->
    receive
        {Number, incoming} ->
            startringing(),
            ringing(Number);
        offhook ->
            start_tone(),
            dial()
    end.

ringing(Number) ->
    receive
        {Number, other onhook} ->
            stop_ringing(),
            idleO;
        {Number, offhook} ->
            stop_ringing(),
            connected(Number)
    end.


start_ringing()-> . . .
start_tone()-> . . .
stop_ringing()-> . . .
