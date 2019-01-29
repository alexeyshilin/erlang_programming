-module(btree).
-export([test/0]).
-export([check/1,insert/2]).
-include("btree.hrl").

node_value(null) ->
	{error, node_value, null};

node_value(#node{val=V, left=L, right=R} = N) when is_record(N, node) ->
	%io:format("[~p]", [N]),
	V;

node_value(N) when is_record(N, node) ->
	%io:format("[~p]", [N]),
	N#node.val.

check(#node{val=V, left=L, right=R} = Tree) when is_record(Tree, node) and (L==null) and (R==null)->
	true;

check(#node{val=V, left=L, right=R} = Tree) when is_record(Tree, node) and not(L==null) and (R==null)->
	LV = node_value(L),
	if
		( LV =< V) ->
			check(L);
		not(LV =< V) ->
			%io:format("*"),
			false
	end;


check(#node{val=V, left=L, right=R} = Tree) when is_record(Tree, node) and (L==null) and not(R==null)->
	RV = node_value(R),
	if
		RV > V ->
			check(R);
		not(RV > V) ->
			%io:format("**"),
			false
	end;

check(#node{val=V, left=L, right=R} = Tree) when is_record(Tree, node) and not(L==null) and not(R==null) ->
	LV = node_value(L),
	RV = node_value(R),
	
	if
		(LV =< V) and (RV > V) ->
			Res1 = check(L),
			Res2 = check(R),

			if 
				(Res1==true) and (Res2==true) ->
					true;
				not((Res1==true) and (Res2==true)) ->
					%io:format("***"),
					false
			end;

		not((LV =< V) and (RV > V)) ->
			%io:format("**** ~p ~p ~p", [LV, V, RV]),
			false
	end;

check(Tree)->
	io:format("!"),
	unsorted.

insert(#node{val=V, left=L, right=R} = N, NewVal) when is_record(N, node)->
	if 
		NewVal>V ->
			RNew = if
				R==null -> #node{val=NewVal,left=null,right=null};
				not(R==null) -> insert(R, NewVal)
			end,
			#node{val=V, left=L, right=RNew};
		not(NewVal>V) ->
			LNew = if
				L==null -> #node{val=NewVal,left=null,right=null};
				not(L==null) -> insert(L, NewVal)
			end,
			#node{val=V, left=LNew, right=R}
	end.

test()->
	%[7,3,2,1,9,5,3,4,8].
	Tree =
	#node{val=7
		,left=#node{val=3
			,left=#node{val=2
				,left=#node{val=1,left=null,right=null}
				,right=null
			}
			,right=#node{val=5
				,left=#node{val=4,left=null,right=null}
				,right=#node{val=6,left=null,right=null}
			}
		}
		,right=#node{val=9
				,left=#node{val=8,left=null,right=null}
				,right=null
		}
	},

	Tree10 =
	#node{val=7
		,left=#node{val=3
			,left=#node{val=2
				,left=null
				,right=null
			}
			,right=#node{val=5
				,left=#node{val=4,left=null,right=null}
				,right=#node{val=6,left=null,right=null}
			}
		}
		,right=#node{val=9
				,left=#node{val=8,left=null,right=null}
				,right=null
		}
	},

	Tree20 =
	#node{val=7
		,left=#node{val=3
			,left=#node{val=2
				,left=#node{val=1,left=null,right=null}
				,right=null
			}
			,right=#node{val=5
				,left=#node{val=4,left=null,right=null}
				,right=null
			}
		}
		,right=#node{val=9
				,left=#node{val=8,left=null,right=null}
				,right=null
		}
	},

	true = check(Tree),
	Tree = insert(Tree10, 1),
	Tree = insert(Tree20, 6),
	ok.
