-module(ponte).
-export([start/0, inicio_travessia_ida/1, inicio_travessia_volta/1, fim_travessia/1]).

start() -> spawn(fun() -> loop(0) end).

loop(Ocupacao) ->
    receive
        {inicio_travessia_ida, From} ->
            if Ocupacao < 10 ->
                From ! {ok, Ocupacao + 1},
                loop(Ocupacao + 1);
               true ->
                From ! {cheia, Ocupacao},
                loop(Ocupacao)
            end;
        {inicio_travessia_volta, From} ->
            if Ocupacao < 10 ->
                From ! {ok, Ocupacao + 1},
                loop(Ocupacao + 1);
               true ->
                From ! {cheia, Ocupacao},
                loop(Ocupacao)
            end;
        {fim_travessia, From} ->
            From ! {ok, Ocupacao - 1},
            loop(Ocupacao - 1)
    end.

inicio_travessia_ida(Pid) ->
    Pid ! {inicio_travessia_ida, self()},
    receive
        {ok, NovaOcupacao} ->
            %io:format("Início travessia ida. Ocupação: ~p~n", [NovaOcupacao]),
            ok;
        {cheia, Ocupacao} ->
            %io:format("Ponte cheia, esperando... Ocupação: ~p~n", [Ocupacao]),
            inicio_travessia_ida(Pid)
    end.

inicio_travessia_volta(Pid) ->
    Pid ! {inicio_travessia_volta, self()},
    receive
        {ok, NovaOcupacao} ->
            %io:format("Início travessia volta. Ocupação: ~p~n", [NovaOcupacao]),
            ok;
        {cheia, Ocupacao} ->
            %io:format("Ponte cheia, esperando... Ocupação: ~p~n", [Ocupacao]),
            inicio_travessia_volta(Pid)
    end.

fim_travessia(Pid) ->
    Pid ! {fim_travessia, self()},
    receive
        {ok, NovaOcupacao} ->
            %io:format("Fim travessia. Ocupação: ~p~n", [NovaOcupacao]),
            ok
    end.