-module(app_tests).

-include_lib("eunit/include/eunit.hrl").

-import(usr_app, [start/2, stop/1]).


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

		,?_assertEqual(ok, application:start(usr))

		,?_assertEqual(ok, usr:add_usr(700000001, 1, prepay))
		,?_assertEqual({ok,{usr,700000001,1,enabled,prepay,[]}}, usr:lookup_id(1))

		,?_assertMatch({ok,"usrDb"}, application:get_env(usr, dets_name))

		,?_assertMatch(Pid when is_pid(Pid), whereis(usr_sup))

		,?_assertEqual(ok, application:stop(usr))

		,?_assertMatch(undefined, whereis(usr_sup))
	].

% erl -pa /var/projects/test/erl/012/usr-1.0/ebin

% c(app_tests).
% app_tests:test().
% eunit:test(app_tests).