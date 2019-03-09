-module(my_db_sup).
-behavior(supervisor).
-export([start_link/0]).
-export([init/1]).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(Data) ->
	Chld = { my_db_gen, {my_db_gen, start, []}, permanent, 30*1000, worker, [my_db_gen] } ,
	{ok, {{one_for_all, 5, 60*60}, [Chld]}}.
	%Response:
	% {ok, {SupervisorSpecification, ChildSpecificationList}}
		% SupervisorSpecification = {RestartStrategy, AllowedRestarts, MaxSeconds}
			% RestartStrategy = one_for_one, one_for_all, rest_for_all
		% ChildSpecificationList = {Id, {Module, Function, Arguments}, Restart, Shutdown, Type, ModuleList}
				% Restart = transient, temporary, permanent
				% Shutdown = brutal kill ... 1 ... infinity
				% Type = worker, supervisor


% c(my_db_sup).
% my_db_sup:start_link().
% whereis(my_db_gen).
% exit(whereis(my_db_gen), kill).
% whereis(my_db_gen).
% my_db_gen:write(foo, bar).
% my_db_gen:read(foo).
% exit(whereis(my_db_gen), kill).
% exit(whereis(my_db_gen), kill).
