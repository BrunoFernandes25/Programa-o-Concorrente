-module(eventos).
-export([start/1, sinaliza/2, espera/5]).

start(E) -> spawn(fun() -> init(E) end).

init(E) ->
    % Inicializa uma lista de contagem com E elementos, todos iniciados com 0
    Contagem = lists:duplicate(E, 0),
    loop(E, Contagem).

loop(E, Contagem) ->
    receive
        {sinaliza, Tipo} ->
            if
                Tipo >= 1 andalso Tipo =< E ->
                    NewContagem = increment_count(Tipo, Contagem),
                    loop(E, NewContagem);
                true ->
                    loop(E, Contagem)
            end;
        {espera, Tipo1, N1, Tipo2, N2, From} ->
            ContagemTipo1 = lists:nth(Tipo1, Contagem),
            ContagemTipo2 = lists:nth(Tipo2, Contagem),
            if
                ContagemTipo1 >= N1 andalso ContagemTipo2 >= N2 ->
                    From ! ok,
                    loop(E, Contagem);
                true ->
                    From ! {wait, Tipo1, N1, Tipo2, N2},
                    loop(E, Contagem)
            end
    end.

sinaliza(Pid, Tipo) ->
    Pid ! {sinaliza, Tipo}.

espera(Pid, Tipo1, N1, Tipo2, N2) ->
    Pid ! {espera, Tipo1, N1, Tipo2, N2,self()},
    receive
        ok -> ok
    end.

% Função para incrementar a contagem de um tipo de evento específico
increment_count(Tipo, Contagem) ->
    lists:sublist(Contagem, Tipo - 1) ++ [lists:nth(Tipo, Contagem) + 1] ++ lists:nthtail(Tipo, Contagem).