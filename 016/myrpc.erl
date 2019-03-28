-module(myrpc).
-export([test/0]).
-export([setup/0, server/0, f/1]).

% server
setup() ->
	spawn('bar@Simon-Thompsons-Computer-2',myrpc,server,[]).

server() ->
	register(facserver,self()),
	facLoop().

facLoop() ->
	receive
		{Pid, N} ->
			Pid ! {ok, fac(N)}
	end,
	facLoop().

fac(0) -> 1;
fac(N) -> N * fac(N-1). 
% /server


% client
f(N) ->
	{facserver, 'bar@Simon-Thompsons-Computer-2'} ! {self(), N},
	%{facserver, 'bar@localhost'} ! {self(), N},
	Res = receive
		{ok, Val} ->
			io:format("Factorial of ~p is ~p.~n", [N,Val]),
			ok
		after 1000 -> timeout
	end,
	Res.
% /client

test()->
	ok.

% sudo hostname Simon-Thompsons-Computer-2
% echo "127.0.0.1	Simon-Thompsons-Computer-2" >> /etc/hosts

% epmd -names
% epmd -daemon

% erl
% code:root_dir().
%% path to otp = code:root_dir() + /lib/jinterface-XXX/priv/OtpErlang.jar
%% path to otp = code:root_dir() + /lib/jinterface-1.8.1/priv/OtpErlang.jar
%% path to otp = /usr/lib64/erlang/lib/jinterface-1.8.1/priv/OtpErlang.jar


%% as server
% erl -sname bar
% c(myrpc).
%
% myrpc:setup().


%% as client
% erl -sname foo
%
% net_kernel:start([foo]).
% erlang:set_cookie(node(), 'cookie-value').
%

% c(myrpc).
%
% myrpc:f(2).
