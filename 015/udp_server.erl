-module(udp_server).


test()->
	ok.

% {ok, Socket} = gen_udp:open(1234,[binary,{header,2}]).

% % wait sendin packets from #2

% flush().

% gen_udp:close(Socket).
