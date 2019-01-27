-module(my_supervisor).
-export([start_link/2, stop/1]).
-export([init/1]).

start_link(Name, ChildSpecList) ->
	register(Name, spawn_link(my_supervisor, init, [ChildSpecList])), ok.

init(ChildSpecList) ->
	process_flag(trap_exit, true),
	loop(start_children(ChildSpecList)).

start_children( [] ) -> [];

start_children([{M, F, A, T} | ChildSpecList]) ->
	case (catch apply(M,F,A)) of
		{ok, Pid} ->
		[{Pid, {M,F,A,T}}|start_children(ChildSpecList)];
		_ ->
			start_children(ChildSpecList)
end.

restart_child(Pid, ChildList) ->
	{value, {Pid, {M,F,A,T}}} = lists:keysearch(Pid, 1, ChildList),
	case T of
		permanent -> 
			io:format("[delete]"),
			lists:keydelete(Pid,1,ChildList);
		transient ->
			io:format("[restart]"),
			{ok, NewPid} = apply(M,F,A),
			[{NewPid, {M,F,A,T}}|lists:keydelete(Pid,1,ChildList)];
		_ -> true
	end.

loop(ChildList) ->
	receive
		{'EXIT', Pid, _Reason} ->
			NewChildList = restart_child(Pid, ChildList),
			loop(NewChildList);
		{stop, From} ->
			From ! { reply , terminate(ChildList)}
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
