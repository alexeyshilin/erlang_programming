-module(my_db_sup_tests).

-include_lib("eunit/include/eunit.hrl").

-import(my_db_sup, [start_link/0]).
-import(my_db_sup, [init/1]).


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

		,?_assertMatch({ok,Pid} when is_pid(Pid), my_db_sup:start_link())
		,?_assertMatch(Pid when is_pid(Pid), whereis(my_db_gen))

		,?_assertMatch({ok, test_val}, my_db_gen:read(test))

		,?_assertMatch(true, exit(whereis(my_db_gen), kill))
		,?_assertMatch(Pid when is_pid(Pid), whereis(my_db_gen))

		,?_assertMatch({ok}, my_db_gen:write(foo, bar))
		,?_assertMatch({ok, bar}, my_db_gen:read(foo))

		,?_assertMatch({error, instance}, my_db_gen:read(baz))
		
		,?_assertMatch(true, exit(whereis(my_db_gen), kill))
		,?_assertMatch(true, exit(whereis(my_db_gen), kill))

		,?_assertMatch(Pid when is_pid(Pid), whereis(my_db_gen))
	].


% erl -pa /var/projects/test/erl/012/mydb-1.0/src

% shell_default:cd("/var/projects/test/erl/012/mydb-1.0/src").
% c(my_db_sup).

% shell_default:cd("/var/projects/test/erl/019/otp/my_db").
% c(my_db_sup_tests).
% my_db_sup_tests:test().
% eunit:test(my_db_sup_tests).
