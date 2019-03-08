%%% Файл: usr.hrl
%%% Описание: Заголовочный файл для бц пользователей
-record(usr, {msisdn,id,status = enabled,plan,services = []}).
%int()
%term()
%atom(), enabled | disabled
%atom(), prepay | postpay
%[atom()], список служебных флагов
