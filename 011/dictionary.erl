-module(dictionary).
-export([dictionary/1, loop/4, server_run/3, firestarter/3]).
-export([cli_server_stop/1]).
-export([cli_start/2, cli_stop/1, cli_is_alive/1, cli_self_node/1, cli_add_node/3, cli_remove_node/2, cli_get_nodes/1, cli_remote_ping/1, cli_remote/1]). % client
-export([cli_db_new/1,cli_db_drop/1]).
-export([cli_set_value/2,cli_get_value/2,cli_remove_value/2, cli_ask_data/1, cli_all_data/1]).
-export([test/0]).
-vsv(0.1).

dictionary(Tablename)->
	Id = ets:new(Tablename, [set, named_table, private, {keypos, 1}]),
	Id.

dictionary_(Id)->
	ets:delete(Id).

server_run(ProcName, NodeName, TableName) ->
	%register(srv, self()),
	%loop().
	%Pid = spawn(dictionary, loop, [[], TableId, TableName]),
	Pid = spawn(dictionary, firestarter, [NodeName, TableName, ProcName]),
	register(ProcName, Pid),

	Pid.

%% Server
add_node([], Node)->
	[Node];

add_node([H|[]]=Nodes, Node)->
	Nodes++[Node];

add_node([H|T]=Nodes, Node)->
	Nodes++[Node];

add_node(Nodes, Node)->
	error.

remove_node([], Node)->
	[];

remove_node([{Node0,Proc}=H|[]], Node)->
	case Node0==Node of
		true -> [];
		false -> [H]
	end;

remove_node([{Node0,Proc}=H|T], Node)->
	case Node0==Node of
		true -> [] ++ remove_node(T, Node);
		false -> [H] ++ remove_node(T, Node)
	end;

remove_node(Nodes, Node)->
	error.

fill_from_list(Table, [])->
	ok;

fill_from_list(Table, [H|[]])->
	ets:insert(Table, H);

fill_from_list(Table, [H|T])->
	ets:insert(Table, H),
	fill_from_list(Table, T).

get_all()->
	get_all(myTable).

get_all(Table)->
	ets:tab2list(Table).

set_value(Data)->
	set_value(myTable, Data).

set_value(Table, Data)->
	ets:insert(Table, Data).

get_value(Key)->
	get_value(myTable, Key).

get_value(Table, Key)->
	ets:lookup(Table, Key).

remove_value(Key)->
	remove_value(myTable, Key).

remove_value(Table, Key)->
	NewTab = ets:match_delete(Table,{Key,'_'}).

remote_call(Proc, Node, Message) ->
	{Proc, Node} ! {self(), test, Message},
	receive
		{ok, Res} -> ok;
		{signal, Pid} -> signal;
		{'EXIT', Pid, Reason} -> exit;
		{nodedown, Node} -> {error, node_down};
		{Pid, CmdResp, Resp} -> Resp;
		{dummy} -> error
		after 1000 -> {error, timeout}
	end.

remote_call(Proc, Node, Action, Data) ->
	{Proc, Node} ! {self(), Action, Data},
	receive
		{ok, Res} -> ok;
		{signal, Pid} -> signal;
		{'EXIT', Pid, Reason} -> exit;
		{nodedown, Node} -> {error, node_down};
		{Pid, CmdResp, Resp} -> Resp;
		{dummy} -> error
		after 1000 -> {error, timeout}
	end.

setup() ->
	%process_flag(trap_exit, true),
	%spawn_link('bar@STC', myproc, server, []),

	Node = 'beta@STC',
	%net_adm:ping(Node)
	%erlang:set_cookie(node(),cake).
	%net_kernel:connect(Node),
	case monitor_node(Node, true) of
		true -> true;
		badarg -> false;
		false -> false
	end,

	ok.

for_each_node(F, []) ->
	[];

for_each_node(F, [H|[]]) ->
	Res = F(H),
	[Res];

for_each_node(F, [H|T]) ->
	Res = F(H),
	[Res] ++ for_each_node(F, T).

response(Pid, Cmd, Data)->
	Pid ! {self(), Cmd, Data}.

firestarter(NodeName, TableName, ProcName)->
	Id = dictionary:dictionary(TableName),

	net_kernel:start([NodeName]),
	erlang:set_cookie(node(), test_cookie_value),

	loop([], Id, TableName, ProcName).

loop(Nodes, TableId, TableName, ProcName)->
	receive
		{ok, Res} -> ok;

		{Pid, db_new, null} when is_pid(Pid) -> 
				TableIdNew = dictionary(TableName),
				Pid ! {self(), start_resp, ok},
				c:flush(),
				loop(Nodes, TableIdNew, TableName, ProcName);
		{Pid, db_drop, null} when is_pid(Pid) -> 
				dictionary_(TableId),
				Pid ! {self(), start_resp, ok},
				c:flush(),
				loop(Nodes, null, null, null);

		{Pid, start, Name} when is_pid(Pid) -> 
				net_kernel:start([Name]),
				erlang:set_cookie(node(), test_cookie_value),
				Pid ! {self(), start_resp, ok},
				c:flush(),
				loop(Nodes, TableId, TableName, ProcName);
		{Pid, stop, null} when is_pid(Pid) -> 
				net_kernel:stop(),
				Pid ! {self(), stop_resp, ok},
				c:flush(),
				loop(Nodes, TableId, TableName, ProcName);
		{Pid, is_alive, null} when is_pid(Pid) -> 
				Pid ! {self(), is_alive_resp, erlang:is_alive()},
				c:flush(),
				loop(Nodes, TableId, TableName, ProcName);

		{Pid, self_node, null} when is_pid(Pid) -> 
				Pid ! {self(), self_node_resp, node()},
				c:flush(),
				loop(Nodes, TableId, TableName, ProcName);

		{Pid, add_node_tmp, NodeNew} when is_pid(Pid) -> 
				io:format("[~p]", [add_node]),
				NewNodes = add_node(Nodes, NodeNew),
				%erlang:set_cookie(node(), test_cookie_value),
				net_kernel:connect(NodeNew),
				net_adm:ping(NodeNew),
				% call remote nodes
				F = fun(Node)-> remote_call(srv, Node, add_node_ntf, NodeNew) end,
				for_each_node(F, Nodes),
				% end
				Pid ! {self(), add_node_resp, ok},
				c:flush(),
				loop(NewNodes, TableId, TableName, ProcName);

		{Pid, add_node, {Node,ProcNameDst}} when (node()==Node) and (ProcName==ProcNameDst) ->
				io:format("[~p]", [add_node]),
				io:format("[skip self node]"),
				Pid ! {self(), add_node_resp, skip},
				c:flush(),
				loop(Nodes, TableId, TableName, ProcName);
		{Pid, add_node, {Node,ProcNameDst}} when is_pid(Pid) -> 
				io:format("[~p]", [add_node]),
				NewNodes = add_node(Nodes, {Node,ProcNameDst}),
				
				%erlang:set_cookie(node(), test_cookie_value),
				net_kernel:connect(Node),
				net_adm:ping(Node),

				monitor_node(Node, true),

				Pid ! {self(), add_node_resp, ok},
				c:flush(),
				loop(NewNodes, TableId, TableName, ProcName);

		{Pid, remove_node_tmp, Node4Remove} when is_pid(Pid) -> 
				io:format("[~p]", [remove_node]),
				NewNodes = remove_node(Nodes, Node4Remove),
				% call remote nodes
				F = fun(Node)-> remote_call(srv, Node, remove_node_ntf, Node4Remove) end,
				for_each_node(F, Nodes),
				% end
				Pid ! {self(), remove_node_resp, ok},
				c:flush(),
				loop(NewNodes, TableId, TableName, ProcName);
		{Pid, remove_node, Node} when is_pid(Pid) -> 
				io:format("[~p]", [remove_node]),
				NewNodes = remove_node(Nodes, Node),

				monitor_node(Node, false),

				Pid ! {self(), remove_node_resp, ok},
				io:format("[~p]", [NewNodes]),
				c:flush(),
				loop(NewNodes, TableId, TableName, ProcName);

		{nodedown, Node} ->
				io:format("[~p ~p]", [nodedown, Node]),
				NewNodes = remove_node(Nodes, Node),
				c:flush(),
				loop(NewNodes, TableId, TableName, ProcName);

		{Pid, get_nodes, Node} when is_pid(Pid) -> 
				io:format("[~p]", [get_nodes]),
				Pid ! {self(), get_nodes_resp, Nodes},
				c:flush(),
				loop(Nodes, TableId, TableName, ProcName);

		{Pid, ping, Params} when is_pid(Pid) -> 
				io:format("[~p]", [remote_ping]),

				%F = fun(Node)-> net_adm:ping(Node) end,
				F = fun({Node,Proc})-> net_adm:ping(Node) end,

				for_each_node(F, Nodes),

				Pid ! {self(), ping_resp, ok},
				c:flush(),
				loop(Nodes, TableId, TableName, ProcName);

		{Pid, all_data, Params} when is_pid(Pid) -> 
				io:format("[~p]", [all_data]),
				response(Pid, all_data_resp, get_all()),
				c:flush(),
				loop(Nodes, TableId, TableName, ProcName);
		{Pid, ask_data, Params} when is_pid(Pid) -> 
				io:format("[~p]", [ask_data]),

				[Node1|_] = Nodes,
				Resp = remote_call(srv, Node1, all_data),
				fill_from_list(TableId, Resp),

				response(Pid, ask_data_resp, get_all()),
				c:flush(),
				loop(Nodes, TableId, TableName, ProcName);

		{Pid, set_value, Params} when is_pid(Pid) -> 
				io:format("[~p]", [set_value]),
				Res = set_value(TableId, Params),
				% call remote nodes
				%F = fun(Node)-> remote_call(srv, Node, set_value_ntf, Params) end,
				F = fun({Node,Proc})-> remote_call(Proc, Node, set_value_ntf, Params) end,
				for_each_node(F, Nodes),
				% end
				response(Pid, set_value_resp, Res),
				c:flush(),
				loop(Nodes, TableId, TableName, ProcName);
		{Pid, set_value_ntf, Params} when is_pid(Pid) -> 
				io:format("[~p]", [set_value]),
				Res = set_value(TableId, Params),
				response(Pid, set_value_ntf_resp, Res),
				c:flush(),
				loop(Nodes, TableId, TableName, ProcName);

		{Pid, get_value, Params} when is_pid(Pid) -> 
				io:format("[~p]", [get_value]),

				%[{Key, Value}] = get_value(TableId, Params)

				Res = case get_value(TableId, Params) of
					{Key, Val} -> Val;
					[{Key, Val}] -> Val;
					[] -> null
				end,

				response(Pid, get_value_resp, Res),
				c:flush(),
				loop(Nodes, TableId, TableName, ProcName);

		{Pid, remove_value, Params} when is_pid(Pid) -> 
				io:format("[~p]", [remove_value]),
				Res = remove_value(TableId, Params),
				% call remote nodes
				%F = fun(Node)-> remote_call(srv, Node, remove_value_ntf, Params) end,
				F = fun({Node,Proc})-> remote_call(Proc, Node, remove_value_ntf, Params) end,
				for_each_node(F, Nodes),
				% end
				response(Pid, remove_value_resp, Res),
				c:flush(),
				loop(Nodes, TableId, TableName, ProcName);
		{Pid, remove_value_ntf, Params} when is_pid(Pid) -> 
				io:format("[~p]", [remove_value]),
				Res = remove_value(TableId, Params),
				response(Pid, remove_value_ntf_resp, Res),
				c:flush(),
				loop(Nodes, TableId, TableName, ProcName);


		{Pid, remote_test, Params} when is_pid(Pid) -> 
				io:format("[~p]", [remote_test]),

				%F = fun(Node)-> remote_call(srv, Node, "some message") end,
				F = fun({Node,Proc})-> remote_call(Proc, Node, "some message") end,

				for_each_node(F, Nodes),

				Pid ! {self(), test_resp, ok},
				c:flush(),
				loop(Nodes, TableId, TableName, ProcName);

		{Pid, test, Params} when is_pid(Pid) -> 
				io:format("[~p]", [test]),
				Pid ! {self(), test_resp, ok},
				c:flush(),
				loop(Nodes, TableId, TableName, ProcName);

		{Pid, server_stop, Params} when is_pid(Pid) -> 
				io:format("[~p]", [server_stop]),
				net_kernel:stop(),
				dictionary_(TableId),
				Pid ! {self(), server_stop_resp, ok},
				c:flush(),
				ok;

		%{signal, Pid} -> signal;
		%{'EXIT', Pid, Reason} -> exit;
		%{nodedown, Node} -> {error, node_down};
		{dummy} -> error
		%after 1000 -> {error, timeout}
	end,
	ok.
%% /Server

%% Client
call(Proc, Cmd, Params)->
	
	Proc ! {self(), Cmd, Params},

	receive
		{Proc, ok} -> ok;
		{Pid, CmdResp, Resp} -> Resp;
		{dummy} -> error;
		Resp -> Resp
		after 1000 -> {error, timeout}
	end.

cli_start(Pid, Name) when is_pid(Pid) ->
	call(Pid, start, Name).

cli_stop(Pid) when is_pid(Pid)->
	call(Pid, stop, null).

cli_is_alive(Pid) when is_pid(Pid)->
	call(Pid, is_alive, null).

cli_self_node(Pid) when is_pid(Pid)->
	call(Pid, self_node, null).

cli_add_node(Pid, Node, ProcName) when is_pid(Pid)->
	call(Pid, add_node, {Node, ProcName}).

cli_remove_node(Pid, Node) when is_pid(Pid)->
	call(Pid, remove_node, Node).

cli_get_nodes(Pid) when is_pid(Pid)->
	call(Pid, get_nodes, null).

cli_remote(Pid) when is_pid(Pid)->
	call(Pid, remote_test, null).

cli_remote_ping(Pid) when is_pid(Pid)->
	call(Pid, ping, null).



cli_set_value(Pid, {Key,Value}) when is_pid(Pid)->
	call(Pid, set_value, {Key,Value}).

cli_get_value(Pid, Key) when is_pid(Pid)->
	call(Pid, get_value, Key).

cli_remove_value(Pid, Key) when is_pid(Pid)->
	call(Pid, remove_value, Key).

cli_db_new(Pid) when is_pid(Pid)->
	call(Pid, db_new, null).

cli_db_drop(Pid) when is_pid(Pid)->
	call(Pid, db_drop, null).


cli_ask_data(Pid)->
	call(Pid, ask_data, null).

cli_all_data(Pid)->
	call(Pid, all_data, null).

cli_server_stop(Pid)->
	call(Pid, server_stop, null).

%% /Client

foreach(F, [H|T]) ->
	F(H),
	foreach(F, T);
foreach(F, []) ->
	ok.


map(F, [H|T]) ->
	[F(H)] ++ map(F, T);
map(F, []) ->
	[].


test_()->
	Pid1 = dictionary:server_run(srv1, some1, myTable1),
	Pid2 = dictionary:server_run(srv2, some2, myTable2),
	Pid3 = dictionary:server_run(srv3, some3, myTable3),

	true = dictionary:cli_is_alive(Pid1),
	true = dictionary:cli_is_alive(Pid2),
	true = dictionary:cli_is_alive(Pid3),

	Node1 = cli_self_node(Pid1),
	Node2 = cli_self_node(Pid2),
	Node3 = cli_self_node(Pid3),

	dictionary:cli_add_node(Pid1, Node1, srv1),
	dictionary:cli_add_node(Pid1, Node2, srv2),
	dictionary:cli_add_node(Pid1, Node3, srv3),

	dictionary:cli_add_node(Pid2, Node1, srv1),
	dictionary:cli_add_node(Pid2, Node2, srv2),
	dictionary:cli_add_node(Pid2, Node3, srv3),

	dictionary:cli_add_node(Pid3, Node1, srv1),
	dictionary:cli_add_node(Pid3, Node2, srv2),
	dictionary:cli_add_node(Pid3, Node3, srv3),

	[{Node2,srv2}, {Node3,srv3}] = cli_get_nodes(Pid1),
	[{Node1,srv1}, {Node3,srv3}] = cli_get_nodes(Pid2),
	[{Node1,srv1}, {Node2,srv2}] = cli_get_nodes(Pid3),

	dictionary:cli_set_value(Pid1, {'some tag 11', "Address 11"}),
	dictionary:cli_set_value(Pid1, {'some tag 12', "Address 12"}),
	dictionary:cli_set_value(Pid1, {'some tag 13', "Address 13"}),

	dictionary:cli_set_value(Pid2, {'some tag 21', "Address 21"}),
	dictionary:cli_set_value(Pid2, {'some tag 22', "Address 22"}),
	dictionary:cli_set_value(Pid2, {'some tag 23', "Address 23"}),

	dictionary:cli_set_value(Pid1, {'some tag 31', "Address 31"}),
	dictionary:cli_set_value(Pid1, {'some tag 32', "Address 32"}),
	dictionary:cli_set_value(Pid1, {'some tag 33', "Address 33"}),

	Res1 = dictionary:cli_get_value(Pid1, 'some tag 11'),
	Res2 = dictionary:cli_get_value(Pid1, 'some tag 22'),
	Res3 = dictionary:cli_get_value(Pid1, 'some tag 33'),
	%[Res1, Res2, Res3].

	"Address 11" = dictionary:cli_get_value(Pid1, 'some tag 11'),
	"Address 22" = dictionary:cli_get_value(Pid1, 'some tag 22'),
	"Address 33" = dictionary:cli_get_value(Pid1, 'some tag 33'),

	"Address 11" = dictionary:cli_get_value(Pid2, 'some tag 11'),
	"Address 22" = dictionary:cli_get_value(Pid2, 'some tag 22'),
	"Address 33" = dictionary:cli_get_value(Pid2, 'some tag 33'),

	"Address 11" = dictionary:cli_get_value(Pid3, 'some tag 11'),
	"Address 22" = dictionary:cli_get_value(Pid3, 'some tag 22'),
	"Address 33" = dictionary:cli_get_value(Pid3, 'some tag 33'),


	dictionary:cli_remove_value(Pid1, 'some tag 11'),

	null = dictionary:cli_get_value(Pid1, 'some tag 11'),
	null = dictionary:cli_get_value(Pid2, 'some tag 11'),
	null = dictionary:cli_get_value(Pid3, 'some tag 11'),

	dictionary:cli_remove_value(Pid2, 'some tag 22'),

	null = dictionary:cli_get_value(Pid1, 'some tag 22'),
	null = dictionary:cli_get_value(Pid2, 'some tag 22'),
	null = dictionary:cli_get_value(Pid3, 'some tag 22'),

	dictionary:cli_remove_value(Pid3, 'some tag 33'),

	null = dictionary:cli_get_value(Pid1, 'some tag 33'),
	null = dictionary:cli_get_value(Pid2, 'some tag 33'),
	null = dictionary:cli_get_value(Pid3, 'some tag 33'),

	"Address 12" = dictionary:cli_get_value(Pid3, 'some tag 12'),
	"Address 13" = dictionary:cli_get_value(Pid2, 'some tag 13'),
	"Address 31" = dictionary:cli_get_value(Pid1, 'some tag 31'),

	dictionary:cli_server_stop(Pid1),
	dictionary:cli_server_stop(Pid2),
	dictionary:cli_server_stop(Pid3),

	ok.

test()->
	Gen1 = fun(Lower, Upper) ->
		%{A1,A2,A3} = now(),
		{A1, A2, A3} = random:seed(erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()),
		random:seed(A1, A2, A3), 
		random:uniform(Upper-Lower)+Lower-1
	end,

	Gen = fun(Lower, Upper, Count) ->
		%{A1,A2,A3} = now(),
		{A1, A2, A3} = random:seed(erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()),
		random:seed(A1, A2, A3), 
		[random:uniform(Upper-Lower)+Lower-1 || _ <- lists:seq(1, Count)]
	end,
	
	F1 = fun(Lower, Upper, Count)-> 
		%lists:map(fun (_) -> random:uniform(Upper-Lower) + Lower end, lists:seq(1,Count))
		lists:map(fun (_) -> random:uniform(Upper-Lower)+Lower-1 end, lists:seq(1,Count))
	end,

	Fatom = fun(L)->
		%list_to_atom(lists:flatten(io_lib:format("zombie~p", [1]))).
		list_to_atom(lists:flatten(L))
	end,

	[Count] = F1(1,10,1),
	
	MainList = [ {Fatom(io_lib:format("srv~p",[X])), Fatom(io_lib:format("some~p",[X])), Fatom(io_lib:format("myTable~p",[X]))} || X <- lists:seq(1, Count)],

	Frun = fun({ProcName, NodeName, TableName})->
		Pid = dictionary:server_run(ProcName, NodeName, TableName),
		Node = dictionary:cli_self_node(Pid),
		{ProcName, NodeName, TableName, Pid, Node}
	end,

	MainList2 = map(Frun, MainList),

	TestIsAlive = fun({ProcName, NodeName, TableName, Pid, Node})->
		true = dictionary:cli_is_alive(Pid),
		ok
	end,

	ok = foreach(TestIsAlive, MainList2),

	FAddNode = fun({ProcName, NodeName, TableName, Pid, Node})->

		FAddNode0 = fun({ProcName0, NodeName0, TableName0, Pid0, Node0})->
			Res = case Node == Node0 of
				true -> dictionary:cli_add_node(Pid, Node0, ProcName0);
				false -> dictionary:cli_add_node(Pid, Node0, ProcName0)
			end,
			ok
		end,

		ok = foreach(FAddNode0, MainList2),
		ok
	end,
	
	ok = foreach(FAddNode, MainList2),
	
	[N] = Gen(1, 1000, 1),

	FAddValue = fun({ProcName, NodeName, TableName, Pid, Node}, X)->
		dictionary:cli_set_value(Pid, {Fatom(io_lib:format("some tag ~p",[X])), io_lib:format("Address ~p", [X])})
	end,

	[ FAddValue(lists:nth(Gen1(1, length(MainList2)+1), MainList2), X) || X <- lists:seq(1,N)],

	FCheckValue = fun({ProcName, NodeName, TableName, Pid, Node}, X)->
		Key = Fatom(io_lib:format("some tag ~p",[X])),
		Val = io_lib:format("Address ~p", [X]),
		Val = dictionary:cli_get_value(Pid, Key)
	end,

	[ FCheckValue(lists:nth(Gen1(1, length(MainList2)+1), MainList2), X) || X <- lists:seq(1,N)],

	SrvStop = fun({ProcName, NodeName, TableName, Pid, Node})->
		ok = dictionary:cli_server_stop(Pid),
		ok
	end,

	ok = foreach(SrvStop, MainList2),

	ok.

% sudo hostname myserver.mydomainname.com
% epmd -daemon
% "127.0.0.1 hostname myserver.mydomainname.com" > /stc/hosts

% c(dictionary).
% dictionary:test().
%
% Id = dictionary:dictionary().
% Pid = dictionary:server_run(srv, Id, myTable).
% Pid = dictionary:server_run(srv, null, null).
%
% dictionary:cli_db_new(Pid).
% dictionary:cli_db_drop(Pid).
% dictionary:cli_db_new(Pid).
%
% false = dictionary:cli_is_alive(Pid).
% ok = dictionary:cli_start(Pid, some1).
% ThisNode = dictionary:cli_self_node(Pid).
% true = dictionary:cli_is_alive(Pid).
%
% dictionary:cli_set_value(Pid, {'some tag 1', "Address 1"}).
% dictionary:cli_set_value(Pid, {'some tag 2', "Address 2"}).
% dictionary:cli_set_value(Pid, {'some tag 3', "Address 3"}).
% dictionary:cli_get_value(Pid, 'some tag 2').
% dictionary:cli_remove_value(Pid, 'some tag 2').
% dictionary:cli_get_value(Pid, 'some tag 2').
%
%
% dictionary:cli_remote_ping(Pid).
%
% dictionary:cli_add_node(Pid, 'some2@localhost.localdomain').
% dictionary:cli_add_node(Pid, 'some1@localhost.localdomain').
% dictionary:cli_remote(Pid).

% dictionary:cli_add_node(Pid, 'some1@myserver.mydomainname.com').
% dictionary:cli_add_node(Pid, 'some2@myserver.mydomainname.com').
% dictionary:cli_add_node(Pid, 'some3@myserver.mydomainname.com').

% erl -name somenode -sname snode -connect_all false
