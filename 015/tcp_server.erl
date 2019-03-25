-module(tcp_server).
-export([server/0, wait_connect/2]).

server()->
	{ok, ListenSocket} = gen_tcp:listen(1234, [binary, {active, false}]),
	wait_connect(ListenSocket,0).

wait_connect(ListenSocket, Count) ->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	Pid = spawn(?MODULE, wait_connect, [ListenSocket, Count+1]),
	get_request(Socket, [], Count).

get_request(Socket, BinaryList, Count) ->
	case gen_tcp:recv(Socket, 0, 5000) of
		{ok, Binary} ->
			get_request(Socket, [Binary|BinaryList], Count);
		{error, closed} ->
			handle(lists:reverse(BinaryList), Count)
	end.

handle(Binary, Count) ->
	{ok, Fd} = file:open("log file_"++integer_to_list(Count), write),
	file:write(Fd, Binary),
	file:close(Fd).

% c(tcp_server).
%
% tcp_server:server().
