
-module(frequency).
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/0]).


%% server

%% Функции инициализации и запуска процесса сервера.
%% Инициализация сервера.
start() ->
	register(frequency, spawn(frequency, init, [])).

init() ->
	Frequencies = {get_frequencies(),[]},
	loop(Frequencies).

% Список частот жёстко закодирован
get_frequencies() -> [10,11,12,13,14,15].

%% server

allocate({[], Allocated}, _Pid) ->
	{{[], Allocated}, {error, nofrequency}};

allocate({[Freq|Free], Allocated}, Pid) ->
	{{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
	NewAllocated=lists:keydelete(Freq, 1, Allocated),
	{[Freq|Free], NewAllocated}.

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
	receive
		{request, Pid, allocate} ->
			{NewFrequencies, Reply} = allocate(Frequencies, Pid),
			reply(Pid, Reply),
			loop(NewFrequencies);
		{request, Pid , {deallocate, Freq}} ->
			NewFrequencies = deallocate(Frequencies, Freq),
			reply(Pid, ok),
			loop(NewFrequencies);
		{request, Pid, stop} ->
			reply(Pid, ok)
	end.

reply(Pid, Reply) ->
	Pid ! {reply, Reply}.
