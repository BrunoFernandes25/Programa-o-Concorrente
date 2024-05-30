-module(eventos).
-export([start/1, sinaliza/2, espera/4]).

start(E) ->
    spawn(fun() -> init(E) end).

init(E) ->
    % Inicializa uma lista de contagem com E elementos, todos iniciados com 0
    Contagem = lists:duplicate(E, 0),
    loop(E, Contagem).

loop(E, Contagem) ->
    receive
        % Recebe uma mensagem para sinalizar um evento do tipo Tipo
        {sinaliza, Tipo} ->
            % Verifica se o tipo de evento está dentro do intervalo esperado
            if
                Tipo >= 1 andalso Tipo =< E ->
                    % Incrementa a contagem do tipo de evento recebido
                    NewContagem = increment_count(Tipo, Contagem),
                    loop(E, NewContagem);
                true ->
                    loop(E, Contagem)
            end;
        % Recebe uma mensagem para esperar até que as condições de evento sejam atendidas
        {espera, Tipo1, N1, Tipo2, N2, From} ->
            % Verifica se as contagens dos tipos de evento esperados são atendidas
            if
                lists:nth(Tipo1, Contagem) >= N1 andalso lists:nth(Tipo2, Contagem) >= N2 ->
                    % Se as condições forem atendidas, envia 'ok' de volta ao remetente
                    From ! ok,
                    loop(E, Contagem);
                true ->
                    % Se as condições não forem atendidas, envia uma mensagem de espera
                    From ! {wait, Tipo1, N1, Tipo2, N2},
                    loop(E, Contagem)
            end
    end.

% Função para sinalizar um evento de um determinado tipo para um processo especificado
sinaliza(Pid, Tipo) ->
    Pid ! {sinaliza, Tipo}.

% Função para solicitar que um processo espere até que condições de eventos específicas sejam atendidas
espera(Pid, Tipo1, N1, Tipo2, N2) ->
    Pid ! {espera, Tipo1, N1, Tipo2, N2,self()},
    receive
        ok -> ok
    end.

% Função para incrementar a contagem de um tipo de evento específico
increment_count(Tipo, Contagem) ->
    lists:sublist(Contagem, Tipo - 1) ++ [lists:nth(Tipo, Contagem) + 1] ++ lists:nthtail(Tipo, Contagem).
