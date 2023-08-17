
Port = erlang:open_port(
       {spawn_executable, "/var/projects/erlang/erlbook/016/rust.001/target/debug/erlang_port_test"},
       [{packet, 4}, nouse_stdio, exit_status, binary]).

%erlang:port_command(Port, term_to_binary("hello")).
%erlang:port_command(Port, term_to_binary(<<"hello">>)).
%erlang:port_command(Port, term_to_binary({echo, <<"hello">>})).

erlang:port_command(Port, term_to_binary(<<"hello">>)).

%flush().

receive
{SomePid, {data,SomeData}} -> 
       io:format("Shell got ~p~n",[SomeData]),
       
       {ok,SomeDataRes} = binary_to_term(SomeData),
       io:format( binary_to_list(SomeDataRes) ),

       SomeStr = binary_to_list(SomeDataRes),
       io:format("Shell got ~s~n",[SomeStr]),
flush()
after 0 -> ok
end.


erlang:port_close(Port).

flush().

