-module(tcp_client).
-export([client/2]).

client(Host, Data) ->
	{ok, Socket} = gen_tcp:connect(Host, 1234, [binary, {packet, 0}]),
	send(Socket, Data),
	ok = gen_tcp:close(Socket).

send(Socket, <<Chunk: 100/binary, Rest/binary>>)->
	gen_tcp:send(Socket, Chunk),
	send(Socket, Rest);

send(Socket, Rest) ->
	gen_tcp:send(Socket, Rest).

% c(tcp_client).
% 
% tcp_client:client({127,0,0,1}, <<"Hello Concurrent World">>).
% tcp_client:client(127.0.0.1, "Test data!!!").