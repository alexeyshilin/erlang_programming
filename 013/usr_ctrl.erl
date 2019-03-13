%%% Файл: usr.erl
%%% Описание: API сервера БД абонентов сети

-module(usr_ctrl).
-export([start/0, start/2, start/3, stop/0, init/4]).
-export([add_usr/3, delete_usr/1, set_service/3, set_status/2, delete_disabled/0, lookup_id/1]).
-export([lookup_msisdn/1, service_flag/2]).
-include("usr.hrl").

-define(TIMEOUT, 30000).

%% Экспортируемые клиентские функции
%% API эксплуатации и технического обслуживания сервера

start() ->
	{exit, "Pid and Node missed."}.

start(SyncNode, SyncPid) ->
	start("usrDb", SyncNode, SyncPid).

start(FileName, SyncNode, SyncPid) ->
	Pid = spawn(?MODULE, init, [FileName, self(), SyncNode, SyncPid]),
	register(?MODULE, Pid),
	receive started-> ok after ?TIMEOUT -> {error, starting} end.

stop() ->
	call(stop).

%% API обслуживания абонентов
add_usr(PhoneNum, Custid, Plan) when Plan==prepay; Plan==postpay ->
	call({add_usr, PhoneNum, Custid, Plan}).

delete_usr(CustId) ->
	call({delete_usr, CustId}).

set_service(CustId, Service, Flag) when Flag==true; Flag==false ->
	call({set_service, CustId, Service, Flag}).

set_status(CustId, Status) when Status==enabled; Status==disabled->
	call({set_status, CustId, Status}).

delete_disabled() ->
	call(delete_disabled).

lookup_id(CustId) ->
	call({lookup_id, CustId}).

%% API служебных приложений

lookup_msisdn(PhoneNo) ->
	call({lookup_msisdn, PhoneNo}).

service_flag(PhoneNo, Service) ->
	call({service_flag, PhoneNo, Service}).

%% Передача сообщений

call(Request) ->
	Ref = make_ref(),
	?MODULE ! {request, {self(), Ref}, Request},
	receive
		{reply, Ref, Reply} -> Reply
	after
		?TIMEOUT -> {error, timeout}
	end.

reply({From, Ref}, Reply) ->
	From ! {reply, Ref, Reply}.

%% Внутренние функции сервера

init(FileName, Pid, SyncNode, SyncPid) ->
	Ref = make_ref(),
	%{SyncPid, SyncNode} ! {request, self(), start},
	{coordinator, SyncNode} ! {request, {self(), Ref}, start},
	Res = receive
		{reply, Ref, Reply} -> Reply
	after
		?TIMEOUT -> {error, timeout}
	end,
	Pid ! started,
	loop(SyncNode, SyncPid).

call_remote(Node, Pid, Request)->
	Ref = make_ref(),
	%{Pid, Node} ! {request, self(), Request},
	{coordinator, Node} ! {request, {self(), Ref}, Request},
	receive
		{reply, Ref, Reply} -> Reply
	after
		?TIMEOUT -> {error, timeout}
	end.

loop(Node, Pid) ->
	receive
		{request, From, stop_sync} ->
			reply(From, ok);
		{request, From, stop} ->
			reply(From, call_remote(Node, Pid, close_tables));
		{request, From, Request} ->
			%Reply = request(Request),
			Reply = call_remote(Node, Pid, Request),
			reply(From, Reply),
			loop(Node, Pid)
	end.


%#1. erl -name zzz -sname zzz
%#2. erl -name mmm -sname mmm

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c(usr_ctrl).
%
%#1&2 rr("usr.hrl").
%
%#1&2  usr_ctrl:start(snode1@myserver, pid(0,0,0)).

% usr_ctrl:add_usr(700000001, 1, prepay).
% usr_ctrl:add_usr(700000002, 2, prepay).
% usr_ctrl:add_usr(700000003, 3, prepay).
% usr_ctrl:lookup_id(1).
% usr_ctrl:lookup_msisdn(700000003).
% usr_ctrl:service_flag(700000003,lbs).
% usr_ctrl:set_service(3, lbs, true).
% usr_ctrl:service_flag(700000003,lbs).

% usr_ctrl:add_usr(700000007, 7, prepay).
% usr_ctrl:add_usr(700000008, 8, prepay).
% usr_ctrl:add_usr(700000009, 9, prepay).

% usr_ctrl:set_status(9, disabled).

%#1 usr_ctrl:set_service(9, lbs, true). % pause 2s
%#2 usr_ctrl:lookup_id(1). % result only after previos command was completed
