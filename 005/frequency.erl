
-module(frequency).
-export([test/0]).
-export([start/0, start/1, start/3, stop/0, allocate/0, deallocate/1]).
-export([init/0]).


%% server

%% Функции инициализации и запуска процесса сервера.
%% Инициализация сервера.

start(Pid) ->
	io:format("[~p]\n", [Pid]),
	register(frequency, Pid),
	Pid.

start(A,B,C) ->
	%Pid = pid(A,B,C),
	%start(Pid).
	ok.

start() ->
	Pid = spawn(frequency, init, []),
	io:format("[~p]\n", [Pid]),
	register(frequency, Pid),
	Pid.

init() ->
	Frequencies = {get_frequencies(),[]},
	loop(Frequencies).

% Список частот жёстко закодирован
get_frequencies() -> [10,11,12,13,14,15].

%% server

allocate({[], Allocated}, _Pid) ->
	{{[], Allocated}, {error, nofrequency}};

allocate({[Freq|Free], Allocated}, Pid) ->
	N = count_by_pid(Allocated, Pid),
	io:format("[count= ~p]\n", [N]),

	if N >= 3 ->
			error_limit;
		N < 3 ->
			{{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}
	end.


% count

count_by_pid([], PidOrig) ->
	0;

count_by_pid([{Freq,Pid}|[]], PidOrig) when Pid==PidOrig ->
	1;

count_by_pid([{Freq,Pid}|[]], PidOrig) when not(Pid==PidOrig) ->
	0;

count_by_pid([{Freq,Pid}|T], PidOrig) when Pid==PidOrig ->
	1+count_by_pid(T, PidOrig);

count_by_pid([{Freq,Pid}|T], PidOrig) when not(Pid==PidOrig) ->
	0+count_by_pid(T, PidOrig);

count_by_pid(A,B) ->
	{count_by_pid, error, [A,B]}.

% /count

deallocate({Free, Allocated}, Freq, Pid) ->
	
	Res = lists:keyfind(Freq, 1, Allocated),

	case Res of
			{Freq2, Pid2} when Pid==Pid2 ->
				io:format("[~p ~p]\n",[Freq2, Pid2]),
				NewAllocated=lists:keydelete(Freq, 1, Allocated),
				{[Freq|Free], NewAllocated};
			{Freq2, Pid2} when not(Pid==Pid2) ->
				io:format("[skip deallocate]\n"),
				Allocated;
			false -> 
				io:format("[not found]\n"),
				Allocated
			end.

%% client

%% Клиентские функции
stop()->call(stop).
allocate()->call(allocate) .
deallocate(Freq)->call({deallocate, Freq}).

%% Мы скрываем формат передачи сообщений за
%% функциональным интерфейсом.
call(Message) ->
	frequency ! {request, self(), Message},
	receive
		{reply, Reply} -> Reply
	end.

%% Цикл обработки
loop(Frequencies) ->
	{F, A} = Frequencies,
	L = length(A),
	%io:format("[~p]\n", [L]),

	receive
		{request, Pid, allocate} ->
			
			%{NewFrequencies, Reply} = allocate(Frequencies, Pid),

			case allocate(Frequencies, Pid) of
				{NewFrequencies, Reply} -> 
					reply(Pid, Reply),
					loop(NewFrequencies);
				error_limit ->
					reply(Pid, error_limit),
					loop(Frequencies)
			end;

		{request, Pid , {deallocate, Freq}} ->
			NewFrequencies = deallocate(Frequencies, Freq, Pid),
			reply(Pid, ok),
			loop(NewFrequencies);
		{request, Pid, stop} when L>0 ->
			io:format("[can't stop - allocated (~p) > 0 ]\n", [L]),
			reply(Pid, not_empty),
			loop(Frequencies);
		{request, Pid, stop} when L==0 ->
			reply(Pid, ok)
	end.

reply(Pid, Reply) ->
	Pid ! {reply, Reply}.

% Test
test()->
	%Pid1 = frequency:start().
	%frequency:allocate().
	%frequency:allocate().
	%Pid2 = frequency:start().
	%frequency:stop().
	ok.
% /Test

% c(frequency).
%
% frequency:start().
% frequency:allocate().
% frequency:allocate().
% frequency:deallocate(11).
% frequency:stop(). 
%
% q().

% PidX = pid(0.81.0).
% frequency:start(PidX).
%