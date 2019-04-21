%%% Файл: usr.erl
%%% Описание : API и код genserver процесса для БД абонентов сети
-module(usr).
-export([start_link/0, start_link/1, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).
-export([add_usr/3, delete_usr/1, set_service/3, set_status/2, delete_disabled/0, lookup_id/1]).
-export([lookup_msisdn/1, service_flag/2]).
-behavior(gen_server).
-include("usr.hrl").

%% Экспортируемые клиентские функции
%% API эксплуатации и технического обслуживания сервера
start_link() ->
	start_link("usrDb").

start_link(FileName) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, FileName, []).

stop() ->
	gen_server:cast(?MODULE, stop).

%% API обслуживания абонентов
add_usr(PhoneNum, CustId, Plan) when Plan==prepay; Plan==postpay ->
	gen_server:call(?MODULE, {add_usr, PhoneNum, CustId, Plan}).

delete_usr(CustId) ->
	gen_server:call(?MODULE, {delete_usr, CustId}).

set_service(CustId, Service, Flag) when Flag==true; Flag==false ->
	gen_server:call(?MODULE, {set_service, CustId, Service, Flag}).

set_status(CustId, Status) when Status==enabled; Status==disabled->
	gen_server:call(?MODULE, {set_status, CustId, Status}).

delete_disabled() ->
	gen_server:call(?MODULE, delete_disabled).

lookup_id(CustId) ->
	usr_db:lookup_id(CustId).

%% API служебных приложений
lookup_msisdn(PhoneNo) ->
	usr_db:lookup_msisdn(PhoneNo).

service_flag(PhoneNo, Service) ->
	case usr_db:lookup_msisdn(PhoneNo) of
		{ok,#usr{services=Services, status=enabled}} ->
			lists:member(Service, Services);
		{ok, #usr{status=disabled}} ->
			{error, disabled};
		{error, Reason} ->
			{error, Reason}
end.

%% Функции обратного вызова
init(FileName) ->
	usr_db:create_tables(FileName),
	usr_db:restore_backup(),
	{ok, null}.

terminate( Reason, LoopData) ->
	usr_db:close_tables().

handle_cast(stop, LoopData) ->
	{stop, normal, LoopData}.

handle_call({add_usr, PhoneNo, CustId, Plan}, From, LoopData) ->
	Reply = usr_db:add_usr(#usr{msisdn=PhoneNo, id=CustId, plan=Plan}),
	{reply, Reply, LoopData};

handle_call({delete_usr, CustId}, From, LoopData) ->
	Reply = usr_db:delete_usr(CustId),
	{reply, Reply, LoopData};

handle_call({set_service, CustId, Service, Flag}, From, LoopData) ->
	Reply = case usr_db:lookup_id(CustId) of
		{ok, Usr} ->
			Services = lists:delete(Service, Usr#usr.services),
			NewServices = case Flag of
				true -> [Service|Services];
				false -> Services
			end,
			usr_db:update_usr(Usr#usr{services=NewServices});
		{error, instance} ->
			{error, instance}
		end,
	{reply, Reply, LoopData};

handle_call({set_status, CustId, Status}, From, LoopData) ->
	Reply = case usr_db:lookup_id(CustId) of
			{ok, Usr} ->
				usr_db:update_usr(Usr#usr{status=Status});
			{error, instance} ->
				{error, instance}
			end,
	{reply, Reply, LoopData};

handle_call(delete_disabled, From, LoopData) ->
	{reply, usr_db:delete_disabled(), LoopData}.


% c(usr).
% c(usr_db).
%
% rr("usr.hrl").
%
% usr:start_link().
% usr:add_usr(700000000, 0, prepay).
% usr:set_service(0, data, true).
% usr:lookup_id(0).
% usr:set_status(0, disabled).
% usr:service_flag(700000000, lbs).
% usr:stop().

% {ok, Pid} = usr:start_link().
% Pid ! hello.

