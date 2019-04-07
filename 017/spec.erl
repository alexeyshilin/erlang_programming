-module(spec).
-export([test/0]).
-export([test_dbg/0]).

test()->
	catch ets:new(tbl, [named_table, ordered_set]),

	ets:insert(tbl, {2, "Germany"}),
	ets:insert(tbl, {3, "Philippines"}),
	ets:insert(tbl, {11, {'EXIT', "some reason 1"}}),
	ets:insert(tbl, {12, {'EXIT', c:pid(0,0,0), "some reason 2"}}),
	ets:insert(tbl, {5, "England"}),

	ok.

test_dbg()->
	dbg:tracer(),

	dbg:n('foo@Simon-Thompsons-Computer-2'),
	List = dbg:ln(),
	io:format("[~p]~n", [List]),

	dbg:p(all, [c]),

	Spec0 = [{['$1', '$2'], [], []}],

	Head1 = ['$1', {'$2', {'EXIT', '_'}}],
	Conditions1 = [],
	Body1 = [],
	Spec1 = {Head1, Conditions1, Body1},

	Head2 = ['$1', {'$2', {'EXIT', '$3', '_'}}],
	Conditions2 = [{is_pid, '$3'}],
	Body2 = [],
	Spec2 = {Head2, Conditions2, Body2},

	dbg:tp({ets, insert, 2}, [Spec1, Spec2]),

	%dbg:stop(),

	ok.

% sudo hostname Simon-Thompsons-Computer-2
% echo "127.0.0.1       Simon-Thompsons-Computer-2" >> /etc/hosts


% c(spec).
%
%#1 erl -sname bar -cookie test
%#2 erl -sname foo -cookie test

%#1 spec:test_dbg().

%#2 spec:test().
