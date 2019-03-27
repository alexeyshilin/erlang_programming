-module(tcp_server).
-export([server/0, server/1, get_request/3]).

server()->
	server(1234).

server(Port)->
	{ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}]),
	wait_connect(ListenSocket,0).

wait_connect(ListenSocket, Count) ->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	Pid = spawn(?MODULE, get_request, [Socket, [], Count]),
	gen_tcp:controlling_process(Socket, Pid),
	wait_connect(ListenSocket, Count+1).

get_request(Socket, BinaryList, Count) ->
	case gen_tcp:recv(Socket, 0, 5000) of
		{ok, Binary} ->
			io:format("[~p]", [Binary]),
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
% tcp_server:server(1234).
%

% tcp_client:client({127,0,0,1}, <<"Hello Concurrent World">>).
% tcp_client:client({127,0,0,1}, "Test data!!!").

% wget http://127.0.0.1:1235/test
% curl http://127.0.0.1:1235/test