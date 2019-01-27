-module(my_db).


-module(echo).
-export([go/0, l o o p / 0 ] ) .
go() ->
Pid = spawn(echo, loop, [ ] ) ,
Pid ! { s e l f O , h e l l o } ,
receive
{Pid, Msg} ->
io:format("~w~n",[Msg])
end,
Pid ! stop.
loop() ->
receive
{From, Msg} ->
From ! { s e l f O , Msg},
loop();
s t o p ->
true
end.


my_db:start () => ok.
my_db:stop() => ok.
my_db:write(Key, Element) => ok.
my_db:delete(Key) => ok.
my_db:read(Key) => {ok, Element} | {error, instance}.
my_db:match(Element) => [Keyl, ... , KeyN].