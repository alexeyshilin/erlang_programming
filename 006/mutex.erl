-module(mutex).
-export([test/0]).
-export([start/0, stop/0]).
-export([wait/0, signal/0]).
-export([init/0]).

start() ->
	Pid = spawn_link(?MODULE, init, []),

	%% ver.1    
%    
%    io:format("[link]\n"),
%
%    Res = try link(Pid) of
%        _ -> ok
%    catch
%        exit:Exit -> {exit, caught, Exit};
%        error:Error -> {error, caught, Error};
%        Throw -> {throw, caught, Throw}
%    end,
%
%    io:format("[link res ~w]\n", [Res]),

	%% ver.2
%    Reference = monitor(process, Pid),


	%if
	%    Res == ok -> register(mutex, Pid);
	%    not(Res == ok) -> Res
	%end.

	register(mutex, Pid),
	{ok, Pid}.

stop() ->
	mutex ! stop,
	unregister(mutex).

wait() ->
	mutex ! {wait, self()},
	receive ok -> ok end.

signal() ->
	mutex ! {signal, self()}, ok.

init() ->
	%process_flag(trap_exit, true),

	%exit("Test"), % test exit
	free().

free() ->
	receive
		{wait, Pid} ->

			%% ver. link
			%io:format("[link]\n"),
			%Res = try link(Pid) of
			%	_ -> ok
			%catch
			%	exit:Exit -> {exit, caught, Exit};
			%	error:Error -> {error, caught, Error};
			%	Throw -> {throw, caught, Throw}
			%end,
			%io:format("[link res ~w]\n", [Res]),

			%% ver. monitor
			Res = monitor(process, Pid),

			Pid ! ok,
			%busy(Pid); %if link
			busy(Pid, Res); %if monitor

		stop ->
			terminate()
	end.

busy(Pid) ->
	receive
		{signal, Pid} ->
			unlink(Pid),
			free();
		{'EXIT', Pid, Reason} ->
			io:format("[got exit - free lock]\n"),
			unlink(Pid),
			free()
		%Res ->
		%	io:format("[got ~w]\n", [Res])
	end.


busy(Pid, Ref) ->
	io:format("[busy ~w ~w]\n", [Pid,Ref]),
	receive
		{signal, Pid} ->
			unlink(Pid),
			free();
		{'DOWN', Ref, process, Pid, Reason} ->
			io:format("[got exit - free lock]\n"),
			demonitor(Ref),
			free()
		%Res ->
		%	io:format("[got ~w]\n", [Res])
	end.


terminate() ->
	receive
		{wait, Pid} ->
			exit(Pid, kill),
			terminate()
	after
		0 -> ok
	end.


%%%


test() ->
	io:format("[start]"),

	mutex:start(),

	Pid1 = spawn(fun () -> mutex:wait(), timer:sleep(1000*2), io:format("Got it 1\n"), mutex:signal() end),
	Pid2 = spawn(fun () -> mutex:wait(), io:format("Got it 2\n"), timer:sleep(1000*1), mutex:signal() end),

	timer:sleep(1000*5), % !!!
	mutex:stop(),

	io:format("[end]"),
	ok.


% c(mutex).
%
% mutex:start().
% mutex:wait().
% mutex:signal().
% mutex:stop().

% c(mutex).
%
% mutex:start().
% mutex:wait().
% spawn(fun () -> mutex:wait(), io:format("Got it\n"), mutex:signal() end).
% mutex:signal().

% c(mutex).
%
% mutex:start().
% spawn(fun () -> mutex:wait(), timer:sleep(1000*5), io:format("Got it 1\n"), mutex:signal() end).
% spawn(fun () -> mutex:wait(), timer:sleep(1000*1), io:format("Got it 2\n"), mutex:signal() end).
% mutex:stop().

% c(mutex).
%
% mutex:test().



% c(mutex).
%
% mutex:start().
% spawn(fun () -> mutex:wait(), timer:sleep(1000*5), exit("Test crash"), io:format("Got it 1\n"), mutex:signal() end).
% spawn(fun () -> mutex:wait(), timer:sleep(1000*1), io:format("Got it 2\n"), mutex:signal() end).
% mutex:stop().

% c(mutex).
%
% mutex:start().
% spawn(fun () -> mutex:wait(), timer:sleep(1000*5), exit("Test crash"), io:format("Got it 1\n"), mutex:signal() end).
% spawn(fun () -> mutex:wait(), timer:sleep(1000*1), io:format("Got it 2\n"), mutex:signal() end).
% mutex:stop().

