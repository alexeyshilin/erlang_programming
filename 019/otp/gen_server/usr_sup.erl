-module(usr_sup).
-behavior(supervisor).
-export([start/0]).
-export([init/1]).

start()->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(FileName) ->
	UsrChild = {usr, {usr, start_link, []}, permanent, 2000, worker, [usr, usr_db]},
	{ok, { {one_for_all, 1, 1 }, [UsrChild]}}.

% c(usr_sup).
%
% usr_sup:start().
% whereis(usr).
% exit(whereis(usr), kill).
% whereis(usr).
% usr:lookup_id(0).
% exit(whereis(usr), kill).
% exit(whereis(usr), kill).

