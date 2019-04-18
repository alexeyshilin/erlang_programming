-module(usr_db).
-include("usr.hrl").
-export( [create_tables/1, close_tables/0, add_usr/1, update_usr/1, delete_usr/1, lookup_id/1, lookup_msisdn/1, restore_backup/0, delete_disabled/0]) .
-include_lib("eunit/include/eunit.hrl").

create_tables(FileName) ->
	ets:new(usrRam, [named_table, {keypos, #usr.msisdn}]),
	ets:new(usrIndex, [named_table]),
	dets:open_file(usrDisk, [{file, FileName}, {keypos, #usr.msisdn}]).

close_tables() ->
	ets:delete(usrRam),
	ets:delete(usrIndex),
	dets:close(usrDisk).

add_usr(#usr{msisdn=PhoneNo, id=CustId} = Usr) ->
	ets:insert(usrIndex, {CustId, PhoneNo}),
	update_usr(Usr).

delete_usr(Usr)->
	Res2 = ets:match_delete(usrRam, Usr),
	ok.

delete_usr(PhoneNo, CustId)->
	Res1 = ets:match_delete(usrIndex, {CustId, PhoneNo}),
	Res2 = ets:match_delete(usrRam, #usr{msisdn=PhoneNo, id=CustId}),
	ok.

update_usr(Usr) ->
	ets:insert(usrRam, Usr),
	dets:insert(usrDisk, Usr),
	ok.

lookup_id(CustId) ->
	case get_index(CustId) of
		{ok,PhoneNo} -> lookup_msisdn(PhoneNo);
		{error, instance} -> {error, instance}
	end.

lookup_msisdn(PhoneNo) ->
	case ets:lookup(usrRam, PhoneNo) of
		[Usr] -> {ok, Usr};
		[] -> { error, instance}
	end.

get_index(CustId) ->
	case ets:lookup(usrIndex, CustId) of
		[{CustId,PhoneNo}] -> {ok, PhoneNo};
		[] -> { error, instance}
	end.

restore_backup() ->
	Insert = fun(#usr{msisdn=PhoneNo, id=Id} = Usr) ->
		ets:insert(usrRam, Usr),
		ets:insert(usrIndex, {Id, PhoneNo}),
		continue
	end,
	dets:traverse(usrDisk, Insert).

delete_disabled() ->
	ets:safe_fixtable(usrRam, true),
	catch loop_delete_disabled(ets:first(usrRam)),
	ets:safe_fixtable(usrRam, false),
	ok.

loop_delete_disabled('$end_of_table' ) ->
	ok;

loop_delete_disabled(PhoneNo) ->
	case ets:lookup(usrRam, PhoneNo) of
		[#usr{status=disabled, id = CustId}] ->
			delete_usr(PhoneNo, CustId);
		_ ->
			ok
	end,
	loop_delete_disabled(ets:next(usrRam, PhoneNo)).

%%% tests %%%

setup1_test_() ->
	{spawn,
		{setup,
		fun () -> create_tables("UsrTabFile") end, 			% init
		fun (_) -> ?cmd("rm UsrTabFile") end,				% clear
		?_assertMatch({error,instance}, lookup_id(1))
		}
	}.


setup2_test_() ->
	{spawn,
		{setup,
		
		fun () -> create_tables("UsrTabFile"),
			Seq = lists:seq(1,100000),
			Add = fun(Id) -> 
					add_usr(#usr{msisdn = 700000000 + Id, id = Id, plan = prepay, services = [data, sms, lbs]})
					end,
			lists:foreach(Add, Seq)
		end,
		
		fun (_) -> ?cmd("rm UsrTabFile") end,
		
		?_assertMatch({ok, #usr{status = enabled}} , lookup_msisdn(700000001) )
		}
	}.

%%% /tests %%%

% c( usr_db ) .
%
% usr_db:test().
% eunit:test(usr_db).
