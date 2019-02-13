-module(sequence).
-export([test/0]).
-export([next/1]).
-vsn(-0).

next(Seq) ->
fun() -> [Seq|next(Seq+1)] end.

test()->
	SeqFun0 = sequence:next(0),
	[Seq1|SeqFun1] = SeqFun0(),
	[Seq2|SeqFun2] = SeqFun1(),
	ok.

% c(sequence).
%
%
% SeqFun0 = sequence:next(0).
% [Seq1|SeqFun1] = SeqFun0().
% [Seq2|SeqFun2] = SeqFun1().
%
% sequence:test().

