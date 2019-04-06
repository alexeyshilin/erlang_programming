%% #1 erl -sname bar
% PortFun = dbg:trace_port(ip, 1234).
% dbg:tracer(port, PortFun).
% dbg:p(all, [c]).
% dbg:tp({ping, '_', '_' } , []).
% dbg:tpl({ping, '_', '_' }, []).
% Pid = ping:start().
% ping:send(Pid).

%% #2 erl -sname foo
% Pid = dbg:trace_client(ip, 1234).
% call ping:start()
% dbg:stop_trace_client(Pid).
