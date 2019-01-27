-module(ring).

-export([test/0]).
-export([proc/5]).
-export([start/3, stop/0]).

% 4-2
% ring:start(M, N, Message).

% server

proc(Firestarter, M, 0, Message, Parent)->
	PidFirestarter = if Firestarter==0 -> self();
						Firestarter >0 -> Firestarter
					end,

	%Pid = spawn(ring, proc, [PidFirestarter, M, N-1, Message]),
	Pid = PidFirestarter,

	io:format("Start ~p from ~p...\n",[Pid, self()]),

	%Res = if Firestarter == 0 -> proc_msg(Pid, M, Message);
	%		Firestarter > 0 -> 0
	%	end,

	myproc(Firestarter, M, 0, Message, Pid, Parent),

	ok;

proc(Firestarter, M, N, Message, Parent)->

	PidFirestarter = if Firestarter==0 -> self();
						Firestarter >0 -> Firestarter
					end,

	Pid = spawn(ring, proc, [PidFirestarter, M, N-1, Message, self()]),

	io:format("Start ~p from ~p.\n",[Pid, self()]),

	%Res = if Firestarter == 0 -> proc_msg(Pid, M, Message);
	%		Firestarter > 0 -> 0
	%	end,


	myproc(Firestarter, M, N, Message, Pid, Parent),

	ok.

myproc(Firestarter, M, N, Message, Pid, Parent) ->
	receive
		{From, Msg, X, Z} when X>0 ->
					%io:format("[test]"),
					%io:format("Receive ~w~n from ~p",[Msg, From]),
					io:format("Receive ~p (#~p) ~w from ~p.\n",[X, self(), Msg, From]),
					%io:format("Firestarter ~p ~p ~p\n", [Firestarter, N, Z]),
					
					if Z>0 ->
						Pid ! {self(), Message, X, Z-1},

						%io:format("Send ~w~n to ~p",[Msg, Pid]),
						io:format("Send (#~p) ~w to ~p.\n",[self(), Msg, Pid]),
						%From ! {self(), "Response"},
						true;
						not(Z>0) ->
							self() ! stop % next we will receive stop command
					end,

					myproc(Firestarter, M, N, Message, Pid, Parent),

					true;
%		{From, Msg, X} when X==1 ->
%					io:format("Receive ~p (#~p) ~w from ~p.\n",[X, self(), Msg, From]),
%					true;
%		{From, Msg, X} when X==2 ->
%					io:format("Receive ~p (#~p) ~w from ~p.\n",[X, self(), Msg, From]),
%					true;
		stop ->
			io:format("Stop ~p\n",[self()]),
			Parent ! stop,
			true
	end.

proc_msg(Pid, 0, Message, N) ->
	Pid ! {self(), Message, 0, N},
	true;

proc_msg(Pid, M, Message, N) ->
	io:format("proc_msg ~p.\n",[M]),

	Pid ! {self(), Message, M, N},

	Res = proc_msg(Pid, M-1, Message, N),

	ok.

% /server



% client

start(M,N,Message)->
	Pid = spawn(ring, proc, [0, M, N, Message, self()]),
	io:format("[start]\n"),

	
	%Pid ! {self(), Message, 1},
	%Pid ! {self(), Message, 1},
	%Pid ! {self(), Message, 2},
	%Pid ! {self(), Message, 3},

	proc_msg(Pid, M, Message, N),

	ok.

stop()->
	ok.

% /client


test()->
	ring:start(2,5,"Test"),
	ring:start(2,5,zorro),
	ok.


% ring:start(2,5,"Test").

