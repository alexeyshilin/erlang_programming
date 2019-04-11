-module(shapes).
-export([test/0]).
-export([perimeter/1, area/1]).
-include("shapes.hrl").


% header

% /header

-spec perimeter(any()) -> number() | error.

perimeter(#circle{radius=R} = Figure) when is_record(Figure,circle) ->
	2*3.1415*R;

perimeter(Figure) when is_record(Figure,rectangle) ->
	2*(Figure#rectangle.width+Figure#rectangle.height);

perimeter(Figure)->
	error.

-spec area(any()) -> ok | error.

area(Figure)->
	error.

-spec test() -> ok.

test()->
	62.830000000000005 = shapes:perimeter(#circle{radius=10, color=white, attributes=[flat, transparent]}),
	34 = shapes:perimeter(#rectangle{width=10,height=7, color=white, attributes=[flat, transparent]}),
	Res = shapes:perimeter(#rectangle{width=10.11,height=7, color=white, attributes=[flat, solid]}), % error
	done. % error


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
