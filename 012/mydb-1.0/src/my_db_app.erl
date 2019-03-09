-module(my_db_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, StartArgs) ->
	my_db_sup:start_link().

stop(_State) ->
	ok.

% c(my_db_app).
%
% code:add_path("mydb-1.0/ebin").
% application:start(my_db).
% my_db_gen:read(test).
% my_db_gen:write(foo, bar).
% my_db_gen:read(foo).
%
% application:get_env(my_db, data).
%
% application:which_applications().
%
% application:get_env(my_db, data).
% application:get_all_env(my_db).
%
% application:get_env(data).
% application:get_all_env().

% appmon:start(). % !!!obsolete!!!
% observer:start().

% systools:make_script("my_db", [local]). %ok
% systools:make_script("my_db", {path, ["mydb-1.0/ebin"]}). % err
% systools:make_script("my_db", {dir, ["mydb-1.0/ebin"]}). % err

% systools:script2boot("my_db").

% erl -boot my_db