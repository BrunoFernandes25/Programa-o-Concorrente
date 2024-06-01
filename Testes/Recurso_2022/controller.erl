-module(controller).
-export([start/0, request_resource/2, release_resource/1]).

-define(MAX_THREADS, 10). % Defina o valor máximo de threads permitidas

% Função para iniciar o servidor
start() -> spawn(fun() -> server(0, 0, 0) end).

server(Res0, Res1, Count) ->
  receive
    {request_resource, Pid, 0} when Count < ?MAX_THREADS ->
      Pid ! {resource_granted, 0},
      server(Res0 + 1, Res1, Count + 1);                      % Atualiza o estado do servidor: incrementa o contador de Res0 e o counter que serve para comparar o numero de threads em uso
    {request_resource, Pid, 1} when Count < ?MAX_THREADS ->
      Pid ! {resource_granted, 1},
      server(Res0, Res1 + 1, Count + 1);
    {release_resource, 0} ->
      server(Res0 - 1, Res1, Count - 1);
    {release_resource, 1} ->
      server(Res0, Res1 - 1, Count - 1);
  % Caso não caiba em nenhum dos padrões acima, mantém o estado atual
    _ ->
      server(Res0, Res1, Count)
  end.

request_resource(Pid, Res) ->
  % Envia uma mensagem ao servidor para solicitar o recurso
  server_pid ! {request_resource, Pid, Res},
  receive
    {resource_granted, Res} ->
      ok
  end.

release_resource(Res) ->
  % Envia uma mensagem ao servidor para liberar o recurso
  server_pid ! {release_resource, Res},
  ok.
