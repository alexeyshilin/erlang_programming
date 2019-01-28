-module(records1).
-export( [ birthday/1 , joe/0 , showPerson/1]).

-record(person, {name,age=0,phone}).
-record(name, {first, surname}).

birthday(#person{age=Age} = P) ->
	P#person{age=Age+1}.

joe() ->
	#person{name="Joe",
			age=21,
			phone="999-999"}.

showPerson(#person{age=Age,phone=Phone,name=Name}) ->
	io:format("name: ~p age: ~p phone: - p - n " , [Name,Age,Phone]).

% c(records1).
%
% rr(records1).
% rl().

% P = #person{name = #name{first = "Robert", surname = "Virding" } }.
% First = (P#person.name)#name.first.
% records1:birthday(records1:joe()).

% record_info(fields, person).
% record_info(size, person).
% #person.phone.
% #person.age.
% #person.name.
%
% compile:file(recordsl, ['E']).