-module(my_db_app_tests).

-include_lib("eunit/include/eunit.hrl").

-import(my_db_app, [start/2, stop/1]).

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

		,?_assertEqual(ok, application:start(my_db))
		,?_assertMatch(Pid when is_pid(Pid), whereis(my_db_sup))

		,?_assertMatch({ok, test_val}, my_db_gen:read(test))
		,?_assertMatch({ok}, my_db_gen:write(foo, bar))
		,?_assertMatch({ok, bar}, my_db_gen:read(foo))

		,?_assertMatch({ok,[{some_key,some_val}]}, application:get_env(my_db, data))
		,?_assertMatch([{included_applications,[]},{data,[{some_key,some_val}]}], application:get_all_env(my_db))
		,?_assertMatch(undefined, application:get_env(data))
		,?_assertMatch([], application:get_all_env())

		,?_assertEqual(ok, application:stop(my_db))
		,?_assertMatch(undefined, whereis(my_db_sup))
	].

% erl

% code:add_path("/var/projects/test/erl/012/mydb-1.0/ebin").

% c(my_db_app_tests).
% my_db_app_tests:test().
% eunit:test(my_db_app_tests).
