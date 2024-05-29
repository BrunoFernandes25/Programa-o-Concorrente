% Define o módulo e as funções exportadas
-module(votacao).
-export([start/0, vote/1, wait_condition/0, loop/3]).

% Função para iniciar o servidor de votação
start() ->  register(votacao, spawn(?MODULE, loop, [0, 0, 0])).   %Votos iniciais (0, 0, 0)
            % e registra este processo com o nome 'votacao'

% Função para votar em um candidato
vote(Candidato) -> votacao ! {vote, Candidato}.

% Função para esperar até que a condição seja satisfeita
wait_condition() ->
    votacao ! {wait, self()},
    receive
        condition_met ->
            io:format("Condição satisfeita: V(c3) > V(c1) e V(c3) > V(c2)~n")
    end.

% Função principal do servidor de votação que gerencia o estado dos votos
loop(C1, C2, C3) ->
    receive
        % Caso receba {vote, c1}, incrementa os votos para c1
        {vote, c1} ->
            NewC1 = C1 + 1,
            io:format("Voto registrado para c1. Total: ~p~n", [NewC1]),
            loop(NewC1, C2, C3);

        % Caso receba {vote, c2}, incrementa os votos para c2
        {vote, c2} ->
            NewC2 = C2 + 1,
            io:format("Voto registrado para c2. Total: ~p~n", [NewC2]),
            loop(C1, NewC2, C3);

        % Caso receba {vote, c3}, incrementa os votos para c3
        {vote, c3} ->
            NewC3 = C3 + 1,
            io:format("Voto registrado para c3. Total: ~p~n", [NewC3]),
            loop(C1, C2, NewC3);

        % Caso receba {wait, From}, verifica se a condição é satisfeita
        {wait, From} ->
            % Verifica se C3 > C1 e C3 > C2
            if
                C3 > C1 andalso C3 > C2 ->
                    % Se a condição for satisfeita, envia 'condition_met' para o processo From
                    From ! condition_met,
                    loop(C1, C2, C3);
                true ->
                    % Se a condição não for satisfeita, chama loop recursivamente com os mesmos valores de votos
                    loop(C1, C2, C3)
            end
    end.