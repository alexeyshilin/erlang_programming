-module(records1).
-export( [ birthday/1 , joe/0 , showPerson/1]).

-record(address, {country, city, street, building, apartment}).
-record(person, {name,age=0,phone,addr}).
-record(name, {first, surname}).

birthday(#person{age=Age} = P) ->
	P#person{age=Age+1}.

joe() ->
	#person{name="Joe",
			age=21,
			phone="999-999"}.

showPerson(#person{age=Age,phone=Phone,name=Name, addr=Address}) ->
	io:format("address: ~p, name: ~p age: ~p phone: ~p~n" , [Address, Name,Age,Phone]).

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


% P = #person{name = #name{first = "Robert", surname = "Virding" }, age=21, addr=#address{apartment=19, building=7, street="Testing", city="Testvill", country="Test"} }.
% P.
% records1:birthday(P).
% records1:showPerson(P).
