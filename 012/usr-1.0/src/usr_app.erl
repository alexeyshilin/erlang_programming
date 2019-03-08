-module(usr_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, StartArgs) ->
	usr_sup:start_link().

stop(_State) ->
	ok.

% erl -pa usr-1.0/ebin

% c(usr_app).
%
% code:add_path("usr-1.0/ebin").
% application:start(usr).
% application:start(usr).
% usr:lookup_id(10).
% application:get_env(usr, dets_name).
% application:stop(usr).
% whereis(usr_sup).
%
% appmon:start()
%
% systools:make_script("usr", [local]).
%