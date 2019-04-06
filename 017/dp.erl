-module(dp).
-compile(export_all).
-export([fill/0, process_msg/0]).


process_msg() ->
	case ets:first(msgQ) of
		'$end_of_table' ->
			ok;
		Key ->
			case ets:lookup(msgQ, Key) of
				[{_, {event, Sender, Msg}}] ->
					event(Sender, Msg);
				[{_, {ping, Sender}}] ->
					ping(Sender)
			end,
			ets:delete(msgQ, Key),
			Key
	end.

event(_,_) -> ok.

ping(_) -> ok.

handle(1, Sender, Msg) -> {event, Sender, Msg};
handle(2, Sender, Msg) -> {ping, Sender};
handle(_Id, _Sender, _Msg) -> {error, unknown_msg}.

handle_msg(<<MsgId, MsgType, Sender: 16, MsgLen, Msg:MsgLen/binary>>) ->
	Element = handle(MsgType, Sender, Msg),
	ets:insert(msgQ, {MsgId, Element}).

fill() ->
	catch ets:new(msgQ, [named_table, ordered_set]),
	dp:handle_msg(<<2,3,0,2,0>>),
	%ets:insert(msgQ, {2, {error,unknown_msg}}),
	ok.

% c(dp).
%
% dp:fill().
% dp:process_msg().

% dbg:tracer().
% Match1 = dbg:fun2ms(fun([_,{_, {error, unknown_msg}}]) -> true end).
% dbg:tp({ets, insert, 2}, Match1).
% dbg:p(all, [c]).
% dp:fill().

% dbg:tracer().
% Match2 = dbg:fun2ms(fun([_, {_, {error, unknown_msg}}]) -> message(caller()) end).
% dbg:tp({ets, insert, 2}, Match2).
% dbg:p(all, [c]).
% dp:fill().

% dbg:tracer().
% Match3 = dbg:fun2ms(fun([Id, Sender, Msg]) when Id /= 1, Id /= 2 -> true end).
% dbg:tpl({dp, handle, 3}, Match3).
% dp:fill().
