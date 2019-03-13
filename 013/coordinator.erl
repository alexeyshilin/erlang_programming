-module(coordinator).
-export([start/0, start/1, stop/0, init/2]).
-export([stop_proc/0]).

%-export([add_usr/3, delete_usr/1, set_service/3, set_status/2, delete_disabled/0, lookup_id/1]).
%-export([lookup_msisdn/1, service_flag/2]).
-include("usr.hrl").

-define(TIMEOUT, 30000).

start() ->
	start("usrDb").

start(FileName) ->
	Pid = spawn(?MODULE, init, [FileName, self()]),
	io:format("[~p]",[Pid]),
	register(?MODULE, Pid),
	receive started-> ok after ?TIMEOUT -> {error, starting} end.

stop_proc() ->
	%unregister(?MODULE),
	%exit(?MODULE, stop_proc).
	%exit(?MODULE).
	Ref= make_ref(),
	From = {self(), Ref},
	?MODULE ! {request, From, stop_proc},
	receive {reply, Ref, Reply} -> Reply after ?TIMEOUT -> {error, stop_proc} end.

stop() ->
	error.

reply({From, Ref}, Reply) ->
	From ! {reply, Ref, Reply}.

%% Внутренние функции сервера

init(FileName, Pid) ->
	usr_db:create_tables(FileName),
	usr_db:restore_backup(),
	Pid ! started,
	loop().

loop() ->
	receive
		{request, From, start} ->
			reply(From, started),
			loop();
		{request, From, start} ->
			reply(From, started),
			loop();

		{request, From, stop_proc} ->
			reply(From, stopped);

		{request, From, test} ->
			reply(From, test);
		{request, From, stop_sync} ->
			reply(From, ok);

		{request, From, stop} ->
			reply(From, usr_db:close_tables());
		{request, From, Request} ->
			io:format("[~p]", [Request]),
			Reply = request(Request),
			reply(From, Reply),
			loop()
	end.

%% Обработка запросов клиента

request({add_usr, PhoneNo, CustId, Plan}) ->
	usr_db:add_usr(#usr{msisdn=PhoneNo,id=CustId,plan=Plan});

request({delete_usr, CustId}) ->
	usr_db:delete_usr(CustId);

request({set_service, CustId, Service, Flag}) ->
	case usr_db:lookup_id(CustId) of
		{ok, Usr} ->
			Services = lists:delete(Service, Usr#usr.services),
			NewServices = case Flag of
							true -> [Service|Services];
							false -> Services
						end,
			usr_db:update_usr(Usr#usr{services=NewServices});
	{error, instance} ->
			{error, instance}
	end;

request({set_status, CustId, Status}) ->
	case usr_db:lookup_id(CustId) of
		{ok, Usr} ->
			usr_db:update_usr(Usr#usr{status=Status});
		{error, instance} ->
			{error, instance}
	end;

request({lookup_id, CustId}) ->
	usr_db:lookup_id(CustId);

request({lookup_msisdn, PhoneNo}) ->
	usr_db:lookup_msisdn(PhoneNo);

request({service_flag, PhoneNo, Service}) ->
	case usr_db:lookup_msisdn(PhoneNo) of
			{ok,#usr{services=Services, status=enabled}} ->
					lists:member(Service, Services);
			{ok, #usr{status=disabled}} ->
					timer:sleep(2*1000),
					{error, disabled};
			{error, Reason} ->
					{error, Reason}
	end;


request(delete_disabled) ->
	usr_db:delete_disabled();

request(test) ->
	test.

% erl -name fcoord1 -sname scoord1

% c(coordinator).
% rr("usr.hrl").
% coordinator:start().
% ...
% coordinator:stop_proc().
