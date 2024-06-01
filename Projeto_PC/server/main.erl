-module(main).
-export([start/1]).

start(Port) ->
  {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {packet, 4}, {active, true}]),
  MatchMaking = spawn(matchmaking, start, []),
  Login = spawn(login, start, []),
  global:register_name(matchmaking_service, MatchMaking),
  global:register_name(login_service, Login),
  accept(ListenSocket).

accept(ListenSocket) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  HandleClient = spawn(fun() -> handle_client(Socket) end),
  gen_tcp:controlling_process(Socket, HandleClient),
  accept(ListenSocket).

send(Socket, Data) ->
  gen_tcp:send(Socket, term_to_binary(Data)).

handle_client(Socket) ->
  LoginPid = global:whereis_name(login_service),
  receive
    {_Pid, ok, login, Lvl, Wins} ->
      send(Socket, {ok, login, Lvl, Wins}),
      handle_client(Socket, Lvl, Wins);
    {_Pid, error, login} ->
      send(Socket, {error, login}),
      handle_client(Socket);
    {_Pid, ok, signup} ->
      send(Socket, {ok, signup}),
      handle_client(Socket);
    {_Pid, error, signup} ->
      send(Socket, {error, signup}),
      handle_client(Socket);
    {_Pid, ok, delete} ->
      send(Socket, {ok, delete}),
      handle_client(Socket);
    {_Pid, error, delete} ->
      send(Socket, {error, delete}),
      handle_client(Socket);
    {_Pid, top, Data} ->
      send(Socket, {ok, top, Data}),
      handle_client(Socket);
    {tcp, _Socket, Data} ->
      case binary_to_term(Data) of
        {top, N} ->
          LoginPid ! {self(), top, N},
          handle_client(Socket);
        {login, User, Pass} ->
          LoginPid ! {self(), login, User, Pass},
          handle_client(Socket);
        {signup, User, Pass} ->
          LoginPid ! {self(), signup, User, Pass},
          handle_client(Socket);
        {delete, User, Pass} ->
          LoginPid ! {self(), delete, User, Pass},
          handle_client(Socket);
        _ ->
          handle_client(Socket)
      end;
    {tcp_closed, _Socket} ->
      ok;
    _ ->
      handle_client(Socket)
  end.

handle_client(Socket, Lvl, Wins) ->
  MatchMakingPid = global:whereis_name(matchmaking_service),
  LoginPid = global:whereis_name(login_service),
  receive
    {Pid, start, Id} ->               
      send(Socket, {ok, start, Id}),
      handle_client(Socket, Lvl, Wins, Pid);
    {_Pid, top, Data} ->
      send(Socket, {ok, top, Data}),
      handle_client(Socket, Lvl, Wins);
    {_Pid, leave} ->
      send(Socket, {ok, leave}),
      handle_client(Socket, Lvl, Wins);
    {tcp, _Socket, Data} ->
      case binary_to_term(Data) of
        join ->
          MatchMakingPid ! {self(), join, Lvl},
          handle_client(Socket, Lvl, Wins);
        logout ->
          LoginPid ! {self(), logout},
          handle_client(Socket);
        {top, N} ->
          LoginPid ! {self(), top, N},
          handle_client(Socket, Lvl, Wins);
        leave ->
          MatchMakingPid ! {self(), leave, Lvl},
          handle_client(Socket, Lvl, Wins);
        _ ->
          handle_client(Socket, Lvl, Wins)
      end;
    {tcp_closed, _Socket} ->
      MatchMakingPid ! {self(), leave},
      LoginPid ! {self(), logout};
    _ ->
      handle_client(Socket, Lvl, Wins)
  end.

handle_client(Socket, Lvl, Wins, GameManager) ->
  LoginPid = global:whereis_name(login_service),
  receive
    {_Pid, update, Data} ->
      send(Socket, {ok, update, Data}),
      handle_client(Socket, Lvl, Wins, GameManager);
    {_Pid, win} ->
      LoginPid ! {self(), win},
      {NewLvl, NewWins} = login:calc(Lvl, lists:max([1, Wins + 1])),
      send(Socket, {ok, win, NewLvl, NewWins}),
      handle_client(Socket, NewLvl, NewWins);
    {_Pid, loss} ->
      LoginPid ! {self(), loss},
      {NewLvl, NewWins} = login:calc(Lvl, lists:min([-1, Wins - 1])),
      send(Socket, {ok, loss, NewLvl, NewWins}),
      handle_client(Socket, NewLvl, NewWins);
    {_Pid, draw} ->
      LoginPid ! {self(), draw},
      send(Socket, {ok, draw, Lvl, 0}),
      handle_client(Socket, Lvl, 0);
    {_Pid, fuel, Val} ->
      send(Socket, {ok, fuel, Val}),
      handle_client(Socket, Lvl, Wins, GameManager);
    {tcp, _Socket, Data} ->
      case binary_to_term(Data) of
        left ->
          GameManager ! {self(), left},
          handle_client(Socket, Lvl, Wins, GameManager);
        right ->
          GameManager ! {self(), right},
          handle_client(Socket, Lvl, Wins, GameManager);
        up ->
          GameManager ! {self(), up},
          handle_client(Socket, Lvl, Wins, GameManager);
        _ ->
          handle_client(Socket, Lvl, Wins, GameManager)
      end;
    {tcp_closed, _Socket} ->
      GameManager ! {self(), leave},
      LoginPid ! {self(), loss},
      LoginPid ! {self(), logout};
    _ ->
      handle_client(Socket, Lvl, Wins, GameManager)
  end.

