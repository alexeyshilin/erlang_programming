
Port = erlang:open_port(
       {spawn_executable, "/bin/cat"},
       [binary, eof, use_stdio, exit_status, stream]).

erlang:port_command(Port, "hello world!").


flush().

erlang:port_close(Port).

flush().

