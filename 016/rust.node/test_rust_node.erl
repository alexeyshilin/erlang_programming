
% epmd -names
% epmd -daemon

% sudo hostname myserver.mydomainname.com
% epmd -daemon
% "127.0.0.1 hostname myserver.mydomainname.com" > /stc/hosts
% "127.0.0.1 hostname myserver" > /stc/hosts

% /usr/local/share/libs/erlang_otp_26.0.2/bin/epmd -daemon

% /var/projects/erlang/erlbook/016/rust.node/target/debug/someport --cookie testcookie

% /usr/local/share/libs/erlang_otp_26.0.2/bin/erl
% /usr/local/share/libs/erlang_otp_26.0.2/bin/erl -sname "blah" -setcookie "testcookie"
% register(srv1,self()).
% {bar, bar@localhost} ! hello.
% {param1, bar@localhost} ! {self(), srv1, "somemessage"}
