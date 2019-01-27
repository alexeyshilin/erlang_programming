-module(my_supervisor).
-export([start_link/2, stop/1]).
-export([init/1]).
-export([start_child/5]).
-export([stop_child/2]).

%%%
-spec get_timestamp() -> integer().

get_timestamp() ->
	%TS = {_,_,Micro} = os:timestamp(),
	%{{Year,Month,Day},{Hour,Minute,Second}} = calendar:now_to_universal_time(TS),
	{Mega, Sec, Micro} = os:timestamp(),
	(Mega*1000000 + Sec)*1000 + round(Micro/1000).



count_since([], Ts) ->
	0;

count_since([H|[]], Ts) ->
	case H >= Ts of
		true ->
			1;
		false ->
			io:format("[* ~w ~w]", [H, Ts]),
			0;
		_ ->
			error
	end;


count_since([H|T], Ts) ->
	case H >= Ts of
		true ->
			1 + count_since(T, Ts);
		false ->
			0 + count_since(T, Ts);
		_ ->
			false
	end;

count_since(_, _) ->
	{error, count_since, 2}.



clear_before([], Ts) ->
	[];

clear_before([H|[]], Ts) ->
	case H >= Ts of
		true ->
			[H];
		false ->
			[];
		_ ->
			error
	end;

clear_before([H|T], Ts) ->
	case H >= Ts of
		true ->
			[H] ++ clear_before(T, Ts);
		false ->
			clear_before(T, Ts);
		_ ->
			false
	end;

clear_before(_, _) ->
	{error, clear_before, 2}.

%%%

start_link(Name, ChildSpecList) ->
	register(Name, spawn_link(my_supervisor, init, [ChildSpecList])), ok.

init(ChildSpecList) ->
	process_flag(trap_exit, true),
	%loop(start_children(ChildSpecList)).
	List = start_children(ChildSpecList, 0),
	loop(List, length(List)+1).

start_children( [], N ) -> [];

start_children([{M, F, A, T} | ChildSpecList], N) ->
	case (catch apply(M,F,A)) of
		{ok, Pid} ->
		Runs = [get_timestamp()],
		[{Pid, {M,F,A,T,Runs, N+1}}|start_children(ChildSpecList, N+1)];
		_ ->
			start_children(ChildSpecList, N)
end.

restart_child(Pid, ChildList) ->
	{value, {Pid, {M,F,A,T,Runs, Id}}} = lists:keysearch(Pid, 1, ChildList),
	case T of
		permanent -> 
			io:format("[delete]"),
			lists:keydelete(Pid,1,ChildList);
		transient ->
			io:format("[restart]"),
			%io:format("[~w]", [Runs]),
			
			Since = get_timestamp() - 1000*60*1,
			C = count_since(Runs, Since),
			%io:format("[count: ~p]", [C]),
			RunsClear = clear_before(Runs, Since),
			%io:format("[clear: ~w]", [RunsClear]),
			%io:format("[count: ~p ~p]", [C, length(RunsClear)]),
			N = length(RunsClear),

			if
				N > 5 -> % remove
					Res = lists:keydelete(Pid,1,ChildList),
					Res;
				not(N>5) -> % restart
					RunsNew = Runs ++ [get_timestamp()],
					%io:format("[~w]", [RunsNew]),
					{ok, NewPid} = apply(M,F,A),
					Res = [{NewPid, {M,F,A,T,RunsNew, Id}}|lists:keydelete(Pid,1,ChildList)],
					Res
			end;

		_ -> true
	end.

%%%
% Client
start_child(Name, Module, Function, Argument, Type) when (Type==permanent) or (Type==transient) ->
	Name ! {start_child, self(), {Module, Function, Argument, Type}},
	receive
			{reply, Id} ->
				Id;
			{error, Reason} ->
				error
	end;

start_child(Name, Module, Function, Argument, Type) ->
	{error, unknown_type, Type}.

% Server
start_child(ChildList, Module, Function, Argument, Type, Id) when (Type==permanent) or (Type==transient) ->
	{ok, NewPid} = apply(Module,Function,Argument),
	[{NewPid, {Module, Function, Argument, Type,[get_timestamp()], Id}} | ChildList];

start_child(ChildList, Module, Function, Argument, Type, Id) ->
	{error, unknown_type, Type}.


stop_child(Id, []) ->
	not_found;

stop_child(Id, [{Pid, {_,_,_,_,_,I}} | []]) when (I==Id) ->
	io:format("[try exit *: ~p ~w]", [I, Pid]),
	unlink(Pid),
	exit(Pid, stop_child),
	[];

stop_child(Id, [H|[]]) ->
	io:format("[- ~p ~w]", [Id, H]),
	[H];

stop_child(Id, [{Pid, {_,_,_,_,_,I}} | T]) when (I==Id) ->
	io:format("[try exit **: ~p ~w]", [I, Pid]),
	unlink(Pid),
	exit(Pid, "Stop child."),
	T;

stop_child(Id, [H|T]) ->
	io:format("[- ~p ~w]", [Id, H]),
	[H] ++ stop_child(Id, T);

% Client
stop_child(Name, Id) ->
	Name ! {stop_child, self(), Id},
	receive
			{reply, Result} ->
				Result;
			{error, Reason} ->
				error
	end.
%%%

loop(ChildList) ->
	loop(ChildList, 1).

loop(ChildList, N) ->
	receive
		{'EXIT', Pid, _Reason} ->
			NewChildList = restart_child(Pid, ChildList),
			loop(NewChildList, N);
		{stop, From} ->
			From ! { reply , terminate(ChildList)};
		{start_child, From, {Module, Function, Argument, Type}} ->
			NewChildList = start_child(ChildList, Module, Function, Argument, Type, N+1),
			From ! { reply , N+1},
			loop(NewChildList, N+1);
		{stop_child, From, Id} ->
			NewChildList = stop_child(Id, ChildList),
			From ! { reply , ok},
			loop(NewChildList, N)
	end.

stop(Name) ->
	Name ! {stop, self()},
	receive {reply, Reply} -> Reply end.

terminate([{Pid, _} | ChildList]) ->
		exit(Pid, kill),
		terminate(ChildList);

terminate( _ChildList) -> ok.

% 1> my_supervisor:start_link(my_supervisor, [{add_two, start, [], permanent}]).
% 1> my_supervisor:start_link(my_supervisor, [{add_two, start, [], transient}]).
% ok
% 2> whereis(add_two).
% <0.125.0>
% 3> exit(whereis(add_two), kill).
% true
% 4> add_two:request(100).
% 102
% 5> whereis(add_two).
% <0.128.0>


% c(my_supervisor).
%
% my_supervisor:start_link(my_supervisor, [{add_two, start, [], permanent}]).
% whereis(add_two).
% exit(whereis(add_two), kill).
% add_two:request(100).
% whereis(add_two).
% my_supervisor:stop().

% c(my_supervisor).
%
% my_supervisor:start_link(my_supervisor, [{add_two, start, [], transient}]).
% whereis(add_two).
% exit(whereis(add_two), kill).
% add_two:request(100).
% whereis(add_two).
% exit(whereis(add_two), kill),whereis(add_two).
% my_supervisor:stop(my_supervisor).



% c(my_supervisor).
%
% my_supervisor:start_link(my_supervisor, [{add_two, start, [], transient}]).
% my_supervisor:start_child(my_supervisor,add_one, start, [], transient).
% whereis(add_one).
% add_one:request(100).
% my_supervisor:stop_child(my_supervisor, 3).
% whereis(add_one).
% my_supervisor:stop(my_supervisor).





% c(my_db).
% c(mutex).
% c(my_supervisor).
%
% my_supervisor:start_link(my_supervisor, [{add_two, start, [], transient}]).
% my_supervisor:start_child(my_supervisor,add_one, start, [], transient).
% my_supervisor:start_child(my_supervisor,mutex, start, [], transient).
% my_supervisor:start_child(my_supervisor,my_db, start, [], transient).
% whereis(add_two).
% whereis(add_one).
% whereis(mutex).
% whereis(my_db).
% exit(whereis(add_two), kill).
% exit(whereis(add_one), kill).
% exit(whereis(mutex), kill).
% exit(whereis(my_db), kill).
% my_supervisor:stop_child(my_supervisor, 4).
% whereis(mutex).
% i().
