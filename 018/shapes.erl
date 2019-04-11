%% @author Some User <someuser@domain.example>
%% @author User Some <usersome@domain.example>
%% @doc Test typer and dialyzer
%% Some info.
%% @reference <a href="http://domain.example/pathto/">link info</a>
%% <em>Some User and User Some</em>
%% Some Users, 2019
%% @copyright 2019 Some User, User Some
%% @version 1.0.0

-module(shapes).
-export([test/0]).
-export([perimeter/1, area/1]).
-include("shapes.hrl").


% header

% /header

%% @doc Calculate perimeter of the figure.
%% More info abount "perimeter".

%% -spec perimeter(any()) -> number() | error.
-spec perimeter(any()) -> number() | error.

perimeter(#circle{radius=R} = Figure) when is_record(Figure,circle) ->
	2*3.1415*R;

perimeter(Figure) when is_record(Figure,rectangle) ->
	2*(Figure#rectangle.width+Figure#rectangle.height);

perimeter(Figure)->
	error.

%% @doc Calculate area of the figure.
%% More info abount "area".

-spec area(any()) -> ok | error.

area(Figure)->
	error.

%% @doc Test action.
%% More info abount "test".

-spec test() -> ok.

test()->
	62.830000000000005 = shapes:perimeter(#circle{radius=10, color=white, attributes=[flat, transparent]}),
	34 = shapes:perimeter(#rectangle{width=10,height=7, color=white, attributes=[flat, transparent]}),
	Res = shapes:perimeter(#rectangle{width=10,height=7, color=white, attributes=[flat, solid]}),
	ok.


% erl
% code:root_dir().
%/usr/lib64/erlang
%/usr/lib64/erlang/lib/kernel-5.4.3.2/ebin
%/usr/lib64/erlang/lib/stdlib-3.4.5.1/ebin
%/usr/lib64/erlang/lib/mnesia-4.15.3.2/ebin

% typer --help
% typer --show shapes.erl shapes.hrl
% /usr/lib64/erlang/bin/typer --show shapes.erl shapes.hrl

%dialyzer --build_plt -r <erl-lib>/kernel-5.4.3.2/ebin <erl-lib>/stdlib-3.4.5.1/ebin <erl-lib>/mnesia-4.15.3.2/ebin
%dialyzer --build_plt -r /usr/lib64/erlang/lib/kernel-5.4.3.2/ebin /usr/lib64/erlang/lib/stdlib-3.4.5.1/ebin /usr/lib64/erlang/lib/mnesia-4.15.3.2/ebin
%dialyzer --build_plt --apps erts kernel stdlib mnesia
%dialyzer --build_plt --no_native --apps erts kernel stdlib mnesia

% erlc +debug_info shapes.erl
% dialyzer -c shapes.erl

% edoc:files(["shapes.erl", "shapes.hrl"], [{includes,["/var/projects/test/erl/018"]}, {preprocess, true}]).
% edoc:files(["shapes.erl", "shapes.hrl"], [{includes,["./"]}, {preprocess, true}]).
% edoc:files(["shapes.erl", "shapes.hrl"], [{includes,[]}, {preprocess, true}]).
% edoc:files(["shapes.erl", "shapes.hrl"]).
