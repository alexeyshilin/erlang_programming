-module(udp_server).


test()->
	ok.

% {ok, Socket} = gen_udp:open(1234,[{active, false}, binary,{header,2}]).

% gen_udp:recv(Socket, 100, 1000*60).

% % wait sendin packets from #2

% gen_udp:close(Socket).
