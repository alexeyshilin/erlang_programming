-module(udp_server).


test()->
	ok.

% {ok, Socket} = gen_udp:open(1234,[{active, false}, binary,{header,2}]).
% inet:getopts(Socket, [active, exit_on_close, header, nodelay]).

% gen_udp:recv(Socket, 100, 1000*60).

% % wait sendin packets from #2

% inet:getstat(Socket).

% gen_udp:close(Socket).
