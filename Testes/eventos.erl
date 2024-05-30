-module(eventos).
-export([start/1, sinaliza/2, espera/4]).

start(E) ->
    spawn(fun() -> init(E) end).

init(E) ->
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
            if
                lists:nth(Tipo1, Contagem) >= N1 andalso lists:nth(Tipo2, Contagem) >= N2 ->
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
    Pid ! {espera, Tipo1, N1, Tipo2, N2},
    receive
        ok -> ok
    end.

increment_count(Tipo, Contagem) ->
    lists:sublist(Contagem, Tipo - 1) ++ [lists:nth(Tipo, Contagem) + 1] ++ lists:nthtail(Tipo, Contagem).
