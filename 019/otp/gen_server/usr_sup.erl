-module(usr_sup).
-behavior(supervisor).
-export([start/0]).
-export([init/1]).

-include_lib("eunit/include/eunit.hrl").

start()->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(FileName) ->
	UsrChild = {usr, {usr, start_link, []}, permanent, 2000, worker, [usr, usr_db]},
	{ok, { {one_for_all, 10, 1 }, [UsrChild]}}.

% c(usr_sup).
%
% usr_sup:start().
% whereis(usr).
% exit(whereis(usr), kill).
% whereis(usr).
% usr:lookup_id(0).
% exit(whereis(usr), kill).
% exit(whereis(usr), kill).

some_test_()->
	{spawn,
		{setup
			,fun() -> ok end % init
			,fun(_) -> ok end % clear

			%test
			, {inorder,
				[
					some_test1()
					,some_test2()
					,some_test3()
				]
			}
		}
	}.

some_test1()->
	Value1 = true,
	Value2 = true,
	[
		?_assertEqual(true, true)
		,?_assertEqual(Value1, Value2)
	].

some_test2()->
	Value1 = true,
	Value2 = true,
	[
		?_assertEqual(true, true)
		,?_assertEqual(Value1, Value2)
	].

some_test3()->
	Value1 = true,
	Value2 = true,
	[
		?_assertEqual(true, true)
		,?_assertEqual(Value1, Value2)

		,?_assertMatch({ok,Pid} when is_pid(Pid), usr_sup:start())
		,?_assertMatch(Pid when is_pid(Pid), whereis(usr))
		,?_assertEqual(true, exit(whereis(usr), kill))
		,?_assertMatch(Pid when is_pid(Pid), whereis(usr))

		,?_assertEqual(ok, usr:add_usr(700000003, 3, prepay))
		,?_assertEqual({ok,{usr,700000003,3,enabled,prepay,[]}}, usr:lookup_id(3))
		
		,?_assertEqual(true, exit(whereis(usr), kill))
		,?_assertEqual(true, exit(whereis(usr), kill))

		,?_assertMatch(Pid when is_pid(Pid), whereis(usr))
	].

% c(usr_sup).
% usr_sup:test().
% eunit:test(usr_sup).
