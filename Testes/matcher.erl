-module(teste_PC_2024).
-export([start/0, waitPlayer/2]).

start()-> spawn(fun() -> match([], []) end).

waitPlayer(Matcher, Role) ->
    Matcher ! {wait, Role, self()},
    receive
        {Matcher, Partida} -> Partida
    end.

match([G1, G2 | GK], P) when length(P) >= 20 ->
    {PlayersM, PlayersNew} = lists:split(20, P),   % O primeiro fica com os que vão jogar, o segundo com os restantes
    Partida = match:new(),
    G1 ! {self(), Partida},
    G2 ! {self(), Partida},
    [P ! {self(), Partida} || P <- PlayersM],
    match(GK, PlayersNew);

match(Gk, P) ->
    receive
        {wait, Role, From} ->
            case Role of
                0 ->
                    match(Gk ++ [From], P);
                1 ->
                    match(Gk, P ++ [From])
            end
    end.
              