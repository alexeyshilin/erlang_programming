-module(shapes).
-export([test/0]).
-export([perimeter/1, area/1]).
-include("shapes.hrl").

perimeter(#circle{radius=R} = Figure) when is_record(Figure,circle) ->
	2*3.1415*R;

perimeter(Figure) when is_record(Figure,rectangle) ->
	2*(Figure#rectangle.width+Figure#rectangle.height);

perimeter(Figure)->
	error.

area(Figure)->
	error.

test()->
	62.830000000000005 = perimeter(#circle{radius=10}),
	34 = perimeter(#rectangle{width=10,height=7}),
	ok.

