-module(usr).
-export([test/0]).
-export([test_start/1, test_stop/1, test_001/1, test_002/1, test_003/1, test_004/1]).
-export([create_tables/0, ensure_loaded/0]).
-export([add_usr/3, delete_usr/1 , set_service/3, set_status/2, delete_disabled/0, lookup_id/1]).
-export([lookup_msisdn/1, service_flag/2]).
-export([start_node/1, add_node/1]).


-include("usr.hrl").

%% Mnesia API
create_tables() ->
	mnesia:create_table(usr, [{disc_copies, [node()]}, {ram_copies, nodes()},
	{type, set}, {attributes,record_info(fields, usr)},
	{index, [id]}]).

ensure_loaded() ->
	%ok = mnesia:wait_for_tables([usr], 60000).
	ok = mnesia:wait_for_tables([usr], 1000).

add_usr(PhoneNo, Custld, Plan) when Plan==prepay; Plan==postpay ->
	Rec = #usr{msisdn = PhoneNo,
		id = Custld,
		plan = Plan},
	Fun = fun() -> mnesia:write(Rec) end,
	{atomic, Res}= mnesia:transaction(Fun),
	Res.

delete_usr(CustId) ->
	F = fun() -> case mnesia:index_read(usr, CustId, id) of
					[] -> {error, instance};
					[Usr] -> mnesia:delete({usr, Usr#usr.msisdn})
				end
		end,
	{atomic, Result} = mnesia:transaction(F),
	Result.

set_service(CustId, Service, Flag) when Flag==true; Flag==false ->
	F = fun() ->
		case mnesia:index_read(usr, CustId, id) of
			[] -> {error, instance};
			[Usr] ->
				Services = lists:delete(Service, Usr#usr.services),
				NewServices = case Flag of
								true -> [Service|Services];
								false -> Services
							end,
				mnesia:write(Usr#usr{services=NewServices})
		end
	end,
	{atomic, Result} = mnesia:transaction(F),
	Result.

set_status(CustId, Status) when Status==enabled; Status==disabled->
	F = fun() ->
		case mnesia:index_read(usr, CustId, id) of
			[] -> {error, instance};
			[Usr] -> mnesia:write(Usr#usr{status=Status})
		end
	end,
	{atomic, Result} = mnesia:transaction(F),
	Result.

delete_disabled() ->
	F = fun() ->
		FoldFun = fun(#usr{status=disabled, msisdn = PhoneNo},_) ->
					mnesia:delete({usr, PhoneNo}),
					ok
					%case mnesia:delete({usr, PhoneNo}) of
					%	{_,_} -> ok
					%end
				end,
		mnesia:foldl(FoldFun, ok, usr)
	end,
	{atomic, ok} = mnesia:transaction(F),
	ok.

lookup_id(CustId) ->
	case mnesia:dirty_index_read(usr, CustId, id) of
		[Usr] -> {ok, Usr};
		[] -> {error, instance}
	end.


%% API служебных приложений

lookup_msisdn(PhoneNo) ->
	case mnesia:dirty_read({usr, PhoneNo}) of
		[Usr] -> {ok, Usr};
		[ ] -> {error, instance}
	end.

service_flag(PhoneNo, Service) ->
	case lookup_msisdn( PhoneNo) of
		{ok,#usr{services=Services, status=enabled}} ->
			lists:member(Service, Services);
		{ok, #usr{status=disabled}} ->
			{error, disabled};
		{error, Reason} ->
			{error, Reason}
	end.

start_node(NodeName)->
	net_kernel:start([NodeName]),
	erlang:set_cookie(node(), test_cookie_value),
	ok.

add_node(NodeNew)->
	net_kernel:connect(NodeNew),
	net_adm:ping(NodeNew),
	ok.

% 001
test_001(Param) when Param == 1 ->
	ok = usr:add_usr(700000001, 1, prepay),
	ok = usr:add_usr(700000002, 2, prepay),
	ok = usr:add_usr(700000003, 3, prepay),
	ok;

test_001(Param) when Param == 2 ->
	ok = usr:add_usr(700000004, 4, prepay),
	ok = usr:add_usr(700000005, 5, prepay),
	ok = usr:add_usr(700000006, 6, prepay),
	ok;

test_001(Param) ->
	{error, test_001}.

% 002
test_002(Param) when Param == 1->
	ok = usr:set_status(1, disabled),
	ok = usr:set_status(6, disabled),
	ok;

test_002(Param) when Param == 2->
	ok = usr:set_service(2, premiumsms, true),
	ok = usr:set_service(5, premiumsms, true),
	ok;

test_002(Param) ->
	{error, test_001}.

% 003
test_003(Param) when (Param == 1) or  (Param == 2) ->
	{ok,#usr{msisdn = 700000003,id = 3,status = enabled, plan = prepay,services = []}} = usr:lookup_id(3),
	{ok,#usr{msisdn = 700000004,id = 4,status = enabled, plan = prepay,services = []}} = usr:lookup_id(4),
	ok.

% 004
test_004(Param) when (Param == 1) or (Param == 2) ->
	true = usr:service_flag(700000002, premiumsms),
	{error,disabled} = usr:service_flag(700000006, premiumsms),
	ok.


test_start(Param)->
	application:start(mnesia),
	usr:create_tables(),

	usr:ensure_loaded(),
	%rr(usr), % only in shell !!!
	ok.

test_stop(Param)->
	application:stop(mnesia),
	%mnesia:stop(),
	ok.

test()->
	error.

% sudo hostname myserver.mydomainname.com
% epmd -daemon
% "127.0.0.1 hostname myserver.mydomainname.com" > /stc/hosts
% "127.0.0.1 hostname myserver" > /stc/hosts

%#1. erl -name somenode1 -sname snode1
%#2. erl -name somenode2 -sname snode2

% c(usr).
%
%#1. usr:start_node(somenode1).
%#1. node().
%
%#2. usr:start_node(somenode2).
%#2. node().
%
%
%#1. net_adm:ping(snode2@myserver).
%#1. nodes().

%#2. net_adm:ping(snode1@myserver).
%#2. nodes().
%
%#1 mnesia:create_schema([node()|nodes()]).
%#2 mnesia:create_schema([node()|nodes()]).


%#1 usr:test_start(1).
%#2 usr:test_start(1).

%#1 ok = usr:test_001(1).
%#2 ok = usr:test_001(2).

%#1 ok = usr:test_002(1).
%#2 ok = usr:test_002(2).

%#1 ok = usr:test_003(1).
%#2 ok = usr:test_003(2).

%#1 ok = usr:test_004(1).
%#2 ok = usr:test_004(2).

%#1 usr:test_stop(1).
%#2 usr:test_stop(2).