-module(login_manager).
-export([start/0, 
        create_account/2,
        close_account/2,
        login/2,
        logout/1,
        online/0]).

start() -> register(?MODULE, spawn(fun() -> loop(#{}) end)).
    %Pid = spawn(fun() -> loop(...) end).           % criar processo que é uma função que faz um loop
    % register(login_manager, Pid).
    % ?MODULE substitui automaticamente pelo login_manager e evita que nos enganemos a escrever


%feito em baixo com o rpc
%create_account(Username, Passwd) ->
%    ?MODULE ! {create_account, Username, Passwd, self()},
%    receive {Result, ?MODULE} -> Result end.        %Pomos ?Module para ter a certeza que o que recebemos é o que queremos

%close_account(Username, Passwd) ->
%    ?MODULE ! {close_account, Username, Passwd, self()},
%    receive {Result, ?MODULE} -> Result end.

%login(Username, Passwd) ->
%    ?MODULE ! {login, username, Passwd, self()},
%    receive {Result, ?MODULE} -> Result end.


%como criar conta login e assim é tudo "igual" podemos criar uma função como a que segue abaixo em vez do que fizemos acima e chamamos rpc(...)
rpc(Request) ->
    ?MODULE ! {Request, self()},
    receive {Result,?MODULE} -> Result end.

create_account(Username,Passwd)-> rpc({create_account,Username,Passwd}).
close_account(Username,Passwd)-> rpc({close_account,Username,Passwd}).
login(Username,Passwd)-> rpc({login,Username,Passwd}).
logout(Username)-> rpc({logout,Username}).
online() ->rpc(online).

loop(Map)->
    receive
        {{create_account, Username, Passwd},From} ->
            io:format("Received create_account request for user ~p~n", [Username]),
            case maps:is_key(Username, Map) of
                true ->                            %existindo falha a criação  
                    From ! {user_exists, ?MODULE}, %enviamos entao erro com user_exists
                    loop(Map);
                false ->
                    From ! {ok, ?MODULE},
                    loop(maps:put(Username,{Passwd,true},Map)) %true serve de flag para saber se tá login ou nao, ao criar conta assumimos que está já login
            end;

        {{close_account, Username, Passwd},From} ->
            %teriamos 3 passos caso exista e acerta a pass ou existe mas nao tem pass correta ou nao existe
            % mas podemos fazer isto em 2 passos se existe e a pass é correta ou então vemos se é errada independentemente do user existir
            case maps:find(Username,Map) of
                {ok,{Passwd,_}} ->           % nao interessa se tá login ou nao, queremos ver se a pass é correta apenas
                    From ! {ok,?MODULE},
                    loop(maps:remove(Username,Map));
                _ ->    % _ qualquer outro caso retorno invalid
                    From ! {invalid, ?MODULE},
                    loop(Map)
            end;

        {{login, Username, Passwd},From} ->
            case maps:find(Username,Map) of
                {ok,{Passwd,false}} -> %caso exista User mas nao esteja login
                    From ! {ok, ?MODULE},    
                    loop(maps:update(Username, {Passwd, true}, Map));     %entao temos passe correta vamos dar login ao User
            _ ->    
                From ! {invalid, ?MODULE},
                loop(Map)
            end;
        {{logout,Username},From} ->
            From ! {ok,?MODULE},
            case maps:find(Username,Map) of
                {ok,{Passwd,true}} ->
                    loop(maps:update(Username, {Passwd, false}, Map));  %se tá login damos logout e booleano fica a falso
            _ ->
                loop(Map)
            end;
        {{online},From} ->
            Users = [Username || {Username, { _, true}} <-maps:to_list(Map)],    %lista de users com login feito nao importa a password
            From ! {Users, ?MODULE},
            loop(Map) % nao sendo feitas alterações fazemos loop(Map) apenas
    end.