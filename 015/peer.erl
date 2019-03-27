-module(peer).
-export([test/0]).
-export([start/0, start/1, connect/1, connect/2, send/1, stop/0]).
-export([loop/1, runthis/3]).

start() ->
	start(1234).

start(Port) ->
	case whereis(srv) of
		undefined ->
			Pid = spawn(?MODULE, loop, [null]),
			register(srv, Pid),

			SomeFunc = fun(Port, Pid)->
				{ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, true}]),
				{ok, Socket} = gen_tcp:accept(ListenSocket),
				io:format("accepted: ~p~n", [Socket]),
				gen_tcp:controlling_process(Socket, Pid),
				Pid ! {self(), connected, Socket}
			end,

			TmpPid = spawn(?MODULE, runthis, [Port, Pid, SomeFunc]),

			ok;
		Pid ->
			{error, already_started}
	end.

runthis(Port, Pid, Func)->
	Func(Port, Pid).

connect(IpAddress) ->
	connect(IpAddress, 1234).

connect(IpAddress, Port) ->
	case whereis(srv) of
		undefined ->
			{error, not_started};
		Pid ->
			{error, already_started}
	end,

	srv ! {self(), connect, IpAddress, Port},

	receive
		{connect_resp, ok} -> ok;
		{connect_resp, Res} -> {error, Res}
		after 1000 -> timeout
	end.

send(String) ->
	case whereis(srv) of
		undefined ->
			{error, not_connected};
		Pid ->
			Pid ! {self(), send, String},
			receive
				{send_resp, ok} -> ok;
				{send_resp, Res} -> {error, Res}
				after 1000 -> timeout
			end
	end.

stop() ->
	case whereis(srv) of
		undefined ->
			{error, not_started};
		Pid ->
			Pid ! {self(), stop},
			receive
				{stop_resp, ok} -> ok;
				{stop_resp, Res} -> {error, Res}
				after 1000 -> timeout
			end
	end.

loop(ListenSocket) ->
	receive
		{tcp, Socket, Msg} ->
			Content = binary_to_list(Msg),
			io:format("Socket # got message: ~p~n", [Content]),
			%gen_tcp:send(Socket, Msg),
			loop(ListenSocket);
		{tcp_closed, _Socket} ->
			io:format("Socket #, session closed ~n"),
			%accept(Id, ListenSocket)
			ok;
		{From, connected, Socket} ->
			io:format("connected ~p~n", [Socket]),
			loop(Socket),
			ok;
		{From, connect, Host, Port} ->
			io:format("[connect]"),
			NewSocket = case gen_tcp:connect(Host, Port, [binary, {packet, 0}]) of
				{ok, SocketTmp} ->
					From ! {connect_resp, ok},
					SocketTmp;
				{error, Reason} ->
					From ! {connect_resp, Reason},
					ListenSocket
			end,
			loop(NewSocket);
		{From, send, Message} ->
			io:format("[send ~p~n]", [ListenSocket]),
			Res = gen_tcp:send(ListenSocket, Message),
			From ! {send_resp, Res},
			loop(ListenSocket);
		{From, stop} ->
			io:format("[stop]"),
			ok = gen_tcp:close(ListenSocket),
			From ! {stop_resp, ok};
		Any ->
			io:format("[any]"),
			{error, Any}
	end.

test()->
	ok.

% c(peer).
%
%
% peer:start( ) -> ok | {error, already_started}
% peer:connect(IpAddress) -> ok | {error, Reason}
% peer:send(String) -> ok | {error, not_connected}
% peer:stop() -> ok | {error, not_started}
%

%1 peer:start(12341).
%2 peer:start(12342).

%1 peer:connect({127,0,0,1}, 12342).
%1 peer:send("Test!").

%2 peer:send("Some message!").
