%%% Файл: usr_db.erl
%%% Описание: API базы данных абонентов

-module(usr_db).
-include("usr.hrl").
-export([init/0, setup/0]).
%-export([create_tables/l, close_tables/0]).
-export([create_tables/1, close_tables/0, add_usr/1, update_usr/1, delete_usr/1, lookup_id/1, lookup_msisdn/1, restore_backup/0, delete_disabled/0]) .
-export([get_index/1]).
-export([delete_usr/2]).

init() ->
	mnesia:create_schema([node()|nodes()]),
	ok.

setup() ->
	Options = [
	 {disc_copies, [node()]}
	,{ram_copies, nodes()}
	,{type, set}
	,{attributes,record_info(fields, usr)}
	,{index, [id]}
	],
	mnesia:create_table(usr, Options),
	ok.

create_tables(FileName) ->
	application:start(mnesia),
	usr_db:setup(),
	ok.

close_tables() ->
	application:stop(mnesia),
	%mnesia:stop(),
	ok.

%dirty_write(Object)
%dirty_read(Objectld)
%dirty_delete(Obj ectld)
%dirty_index_read(Table, SecondaryKey, Attribute)

add_usr(#usr{msisdn=PhoneNo, id=CustId} = Usr) ->
	mnesia:dirty_write(Usr).

delete_usr(Usr) when is_record(Usr, usr) ->
	mnesia:dirty_delete(Usr#usr.id).

delete_usr(PhoneNo, CustId)->
	Res = mnesia:dirty_delete({usr, PhoneNo}),
	Res.

update_usr(Usr) ->
	mnesia:dirty_write(Usr).

lookup_id(CustId) ->
	case mnesia:dirty_index_read(usr, CustId, id) of
		[Usr] -> {ok, Usr};
		[] -> { error, instance}
	end.

lookup_msisdn(PhoneNo) ->
	case mnesia:dirty_read({usr, PhoneNo}) of
		[Usr] -> {ok, Usr};
		[] -> { error, instance}
	end.

get_index(CustId) ->
	case mnesia:dirty_index_read(usr, CustId, id) of
		%[Usr] -> {ok, Usr#usr.msisdn};
		[#usr{id=CustId,msisdn=PhoneNo}] -> {ok, PhoneNo};
		[] -> { error, instance}
	end.

restore_backup() ->
	ok = mnesia:wait_for_tables([usr], 1000).

delete_disabled() ->
	% fixtable ???
	catch loop_delete_disabled(mnesia:dirty_first(usr)),
	% unfixtable ???
	ok.

loop_delete_disabled('$end_of_table' ) ->
	ok;

loop_delete_disabled(PhoneNo) ->
	case mnesia:dirty_read({usr, PhoneNo}) of
		[#usr{status=disabled, id = CustId}] ->
			delete_usr(PhoneNo, CustId);
		_ ->
			ok
	end,
	loop_delete_disabled(mnesia:dirty_next(usr, PhoneNo)).


% usr_db:create_tables("UsrTabFile").
% ets:info(usrIndex).
% ets:info(usrRam).
% dets:info(usrDisk).
% usr_db:close_tables().
% dets:info(usrDisk).
% ets:info(usrRam).
% ets:info(usrIndex).

% c(usr db).
% rr( "usr.hrl" ).
% usr db:create_tables("UsrTabFile").
% usr_db:lookup_id(1).
% Seq = lists:seq(1, 100000).
% Add = fun(Id) -> usr_db:add_usr(#usr{msisdn = 700000000 + Id,id = Id,plan = prepay,services = [data, sms, lbs]}) end.
% lists:foreach(Add, Seq).
% ets:info(usrRam).
% {ok, UsrRec} = usr_db:lookup_msisdn(700000001).
% usr_db:update_usr(UsrRec#usr{services = [data, sms], status = disabled}).
% usr_db:lookup_msisdn(700000001).

% exit(self(), kill).
% usr_db:lookup_msisdn(700000001).
% usr_db:create_tables("UsrTabFile").
% usr_db:lookup_msisdn(700000001).
% dets:lookup(usrDisk,700000001).

% c(usr_db).
% usr_db:restore_backup().
% usr_db:lookup_msisdn(700000001).
% usr_db:lookup_id(1).

% c(usr_db).
% usr_db:delete_disabled().
% ets:info(usrRam).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% c(usr_db).

% sudo hostname myserver.mydomainname.com
% epmd -daemon
% "127.0.0.1 hostname myserver.mydomainname.com" > /stc/hosts
% "127.0.0.1 hostname myserver" > /stc/hosts
        
%#1. erl -name somenode1 -sname snode1
%#2. erl -name somenode2 -sname snode2

%#1 erlang:is_alive().
%#2 erlang:is_alive().

%#1. net_adm:ping(snode2@myserver).
%#1. nodes().

%#2. net_adm:ping(snode1@myserver).
%#2. nodes().

%rem%%%%#1 net_kernel:start([foo]).
%rem%%%%#1 erlang:set_cookie(node(), test_cookie_value).

%rem%%%%#2 net_kernel:start([bar]).
%rem%%%%#2 erlang:set_cookie(node(), test_cookie_value).

%#1 usr_db:init().
%#1 usr_db:create_tables(null).
%#2 usr_db:create_tables(null).

%#1 usr_db:restore_backup().
%#2 usr_db:restore_backup().

%#1 rr(usr).
%#2 rr(usr).

% usr_db:add_usr(#usr{msisdn=700000001, id=1, plan=prepay}).
% usr_db:add_usr(#usr{msisdn=700000002, id=2, plan=postpay}).
% usr_db:add_usr(#usr{msisdn=700000003, id=3, plan=prepay}).

% usr_db:lookup_id(1).
% usr_db:lookup_id(2).
% usr_db:lookup_id(3).

% usr_db:lookup_msisdn(700000001).
% usr_db:lookup_msisdn(700000002).
% usr_db:lookup_msisdn(700000003).

% usr_db:get_index(1).
% usr_db:get_index(2).
% usr_db:get_index(3).

% usr_db:update_usr(#usr{msisdn=700000002, id=2, plan=prepay, status=disabled}).

% usr_db:delete_disabled().

% usr_db:lookup_id(2).

% usr_db:delete_usr(700000003,3).

% usr_db:lookup_id(3).

%#1 usr_db:close_tables().
%#2 usr_db:close_tables().

