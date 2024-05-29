-module(jogoImp).
-export([start/0, participa/1, adivinha/2]).

% Inicia o servidor do jogo
start() -> spawn(fun() -> jogo([]) end).

% Jogador participa de um jogo
participa(Jogo) -> 
    Jogo ! {participa, self()},     % Enviamos o seu pid para garantir que a resposta vem mesmo para nós
    receive 
        {Partida, Jogo} -> Partida 
    end.

% Jogador faz uma tentativa de adivinhação
adivinha(Partida, Numero) ->
    Partida ! {adivinha, Numero, self()},
    receive 
        {Res, Partida} -> Res 
    end.

% Gerencia o estado do jogo aguardando jogadores
jogo(Jogadores) when length(Jogadores) < 4 ->
    receive 
        {participa, From} ->
            jogo([From | Jogadores])
    end;

% Quando há jogadores suficientes, inicia uma nova partida
jogo(Jogadores) ->              
    Numero = random:uniform(100),
    Partida = spawn(fun() -> partida(Numero, 1, false, false) end),
    [Jogador ! {Partida, self()} || Jogador <- Jogadores],
    spawn(fun() -> receive after 60000 -> Partida ! timeout end end),    % Após um minuto manda mensagem à partida para ela terminar
    jogo([]).

% Gerencia o estado de uma partida
partida(Numero, Tentativas, Timeout, Ganhou) ->
    receive
        {adivinha, N, From} -> 
            Res =
            if
                Ganhou -> "PERDEU";
                Timeout -> "TEMPO";
                Tentativas >= 100 -> "TENTATIVAS";
                Numero < N -> "MENOR";
                Numero > N -> "MAIOR";
                true -> "GANHOU"
            end,
            From ! {Res, self()},
            partida(Numero, Tentativas + 1, Timeout, Ganhou orelse Res =:= "GANHOU");
        timeout -> 
            partida(Numero, Tentativas, true, Ganhou)
    end.