%% @type color() = red | white | blue. The color of the shapes.
%% @type attr() = solid | flat | transparent. Shapes attributes.

-type(color() :: red | white | blue).

-type(attr() :: solid | flat | transparent).

-record(circle, {
	radius	::integer()
	,color ::color()
	,attributes	::[attr()]
	}).

-record(rectangle, {
	width	::integer()
	,height	::integer()
	,color ::color()
	,attributes	::[attr()]
	}).
