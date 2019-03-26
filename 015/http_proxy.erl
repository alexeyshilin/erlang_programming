-module(http_proxy).
%-behaviour(application).
%-behavior(gen_server).
-export([test/0]).
-export([start/1, wait_connect/2]).
-vsv(0.1).
%-include("http_proxy.hrl").
%-include_lib("wx/include/wx.hrl").

start(Port)->
	{ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false},{reuseaddr, true}]),
	wait_connect(ListenSocket,0),
	ok.

wait_connect(ListenSocket, Count) ->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	%ok = inet:setopts(Socket, [{packet,http_bin}]),
	Pid = spawn(?MODULE, wait_connect, [ListenSocket, Count+1]),
	passive_loop(Socket, [], Count).
	%active_loop(Socket).

headers2txt([])->
	[];

headers2txt([{Key,Value}|[]])->
	Key ++ ": " ++ Value ++ "\r\n";

headers2txt([{Key,Value}|T])->
	Key ++ ": " ++ Value ++ "\r\n" ++ headers2txt(T);

headers2txt(Any)->
	{error, headers2txt}.


get_url([H|T]) when not(H==32)->
	get_url(T);

get_url([H|T]) when (H==32) ->
	get_url_(T);

get_url(Any)->
	{error, get_url, Any}.


get_url_([H|T]) when not(H==32) ->
	[H] ++ get_url_(T);

get_url_([H|T]) when (H==32) ->
	[];

get_url_(Any)->
	{error, get_url_, Any}.

passive_loop(Socket, BinaryList, Count) ->
	case gen_tcp:recv(Socket, 0, 1000*60) of
		{ok, Binary} ->
			%io:format("[~p]", [Binary]),

			Content = binary_to_list(Binary),
			%Offset = string:str(Content, "\r\n\r\n"),
			%io:format("[~p]", [Content]),

			%Url = "http://domain.example/",
			Url = get_url(Content),

			io:format("[~p]", [Url]),

			inets:start(),
			Res = httpc:request(Url),
			{ok, {{Proto,Code,RespTxt}, Headers, Data}} = Res,
			%io:format("[~p]", [Res]),
			%gen_tcp:send(Socket, "Test message!"), gen_tcp:close(Socket),
			gen_tcp:send(Socket, headers2txt(Headers)++"\r\n"),
			gen_tcp:send(Socket, Data),
			gen_tcp:close(Socket),
			passive_loop(Socket, [Binary|BinaryList], Count), %???
			ok;
		{error,timeout} ->
			io:format("[~p]", ["timeout"]),
			{error,timeout};
		{error, closed} ->
			handle(lists:reverse(BinaryList), Count)
	end.

active_loop(Socket) ->
	receive
	{tcp, Socket, Bin} ->
		
		%io:format("[~p]", [Bin]),

		Content = binary_to_list(Bin),

		%Url = "http://domain.example/",
		Url = get_url(Content),

		io:format("[~p]", [Url]),

		inets:start(),
		Res = httpc:request(Url),
		{ok, {{Proto,Code,RespTxt}, Headers, Data}} = Res,
		%io:format("[~p]", [Res]),
		%gen_tcp:send(Socket, "Test message!"), gen_tcp:close(Socket),
		gen_tcp:send(Socket, headers2txt(Headers)++"\r\n"),
		gen_tcp:send(Socket, Data),
		gen_tcp:close(Socket),

		active_loop(Socket);
	{tcp_closed, Socket} ->
		io:format("Server socket closed~n")
	end.


handle(Binary, Count) ->
	{ok, Fd} = file:open("log file_"++integer_to_list(Count), write),
	file:write(Fd, Binary),
	file:close(Fd).

test()->
	ok.

% c(http_proxy).
%
% http_proxy:start(1500).

% curl http://domain.example/test --proxy http://127.0.0.1:1500
