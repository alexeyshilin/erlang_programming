-module(fac).
-export([call/1]).

call(X) ->
	{any, 'c1@Simon-Thompsons-Computer-2'} ! {self(), X},
	receive
		{ok, Result} ->
			Result
	end.

% sudo hostname Simon-Thompsons-Computer-2
% echo "127.0.0.1	Simon-Thompsons-Computer-2" >> /etc/hosts

%#1 erl -sname "blah" -setcookie "refactorcookie"

%#2 fac

%#1
% c(fac).
%
% fac:call(7).