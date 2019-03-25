-module(udp_client).

test()->
	ok.

% {ok, Socket} = gen_udp:open(1235).
% flush().

% gen_udp:send(Socket, {127,0,0,1}, 1234, <<"Hello World">>).
% gen_udp:send(Socket, {127,0,0,1}, 1234, "Hello World").

% gen_udp:close(Socket).
