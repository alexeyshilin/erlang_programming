-module(my_db_gen_tests).

-include_lib("eunit/include/eunit.hrl").

-import(my_db_gen, [start_link/0, start_link/1]).
-import(my_db_gen, [init/1, terminate/2, handle_call/3, handle_cast/2]).
-import(my_db_gen, [start/0, start/1, stop/0, write/2, delete/1, read/1, match/1]).



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

		,?_assertMatch({ok, Pid} when is_pid(Pid), my_db_gen:start([{test, test_val}]))

		,?_assertMatch({ok, test_val}, my_db_gen:read(test))
		,?_assertMatch({ok}, my_db_gen:write(foo, bar))
		,?_assertEqual({error, instance}, my_db_gen:read(baz))
		,?_assertEqual({ok, bar}, my_db_gen:read(foo))
		,?_assertEqual([foo], my_db_gen:match(bar))

		,?_assertEqual(ok, my_db_gen:stop())
	].

% erl -pa /var/projects/test/erl/012/mydb-1.0/src

% shell_default:cd("/var/projects/test/erl/012/mydb-1.0/src").
% c(my_db_gen).

% shell_default:cd("/var/projects/test/erl/019/otp/my_db").
% c(my_db_gen_tests).
% my_db_gen_tests:test().
% eunit:test(my_db_gen_tests).
