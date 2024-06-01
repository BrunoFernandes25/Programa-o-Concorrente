-module(login).
-export([start/0, calc/2]).

save_to_file(Data) ->
  file:write_file(".data", term_to_binary(Data)).

load_from_file() ->
  case file:read_file(".data") of
    {ok, BinaryData} ->
      {ok, binary_to_term(BinaryData)};
    X -> X
  end.

calc(Lvl, Wins) ->
  if
    Lvl =< Wins -> {Lvl + 1, 0};
    (-Wins >= Lvl / 2) and (Lvl > 1) -> {Lvl - 1, 0};
    true -> {Lvl, Wins}
  end.

start() ->
  timer:send_interval(5000, {self(), save}),
  case load_from_file() of
    {ok, {Data, Usernames}} ->
      login_service(Data, Usernames, maps:new());
    {error, _} ->
      login_service(maps:new(), sets:new(), maps:new())
  end.

login_service(Data, Usernames, SessionManager) ->     
  receive
    {Pid, login, User, Pass} ->
      case maps:find({User, Pass}, Data) of
        {ok, {Lvl, Wins}} ->
          Pid ! {self(), ok, login, Lvl, Wins},
          logger:notice("~p logged in", [User]),
          login_service(Data, Usernames, maps:put(Pid, {User, Pass}, SessionManager));
        error ->
          Pid ! {self(), error, login},
          login_service(Data, Usernames, SessionManager)
      end;
    {Pid, signup, User, Pass} ->
      case sets:is_element(User, Usernames) of
        true ->
          Pid ! {self(), error, signup},
          login_service(Data, Usernames, SessionManager);
        false ->
          Pid ! {self(), ok, signup},
          logger:notice("User ~p created", [User]),
          login_service(maps:put({User, Pass}, {1, 0}, Data), sets:add_element(User, Usernames), SessionManager)
      end;
    {Pid, delete, User, Pass} ->
      case maps:is_key({User, Pass}, Data) of
        true ->
          Pid ! {self(), ok, delete},
          login_service(maps:remove({User, Pass}, Data), sets:del_element(User, Usernames), maps:remove(Pid, SessionManager));
        false ->
          Pid ! {self(), error, delete},
          login_service(Data, Usernames, SessionManager)
      end;
    {Pid, logout} ->
      case maps:find(Pid, SessionManager) of
        {ok, {User, _Pass}} ->
          logger:notice("~p logged out", [User]),
          login_service(Data, Usernames, maps:remove(Pid, SessionManager));
        error ->
          login_service(Data, Usernames, SessionManager)
      end;
    {Pid, win} ->
      case maps:find(Pid, SessionManager) of
        {ok, {User, Pass}} ->
          case maps:find({User, Pass}, Data) of
            {ok, {Lvl, Wins}} ->
              logger:notice("User ~p won a game", [User]),
              New = lists:max([1, Wins + 1]),
              login_service(maps:update({User, Pass}, calc(Lvl, New), Data), Usernames, SessionManager);
            error ->
              login_service(Data, Usernames, SessionManager)
          end;
        error ->
          login_service(Data, Usernames, SessionManager)
      end;
    {Pid, loss} ->
      case maps:find(Pid, SessionManager) of
        {ok, {User, Pass}} ->
          case maps:find({User, Pass}, Data) of
            {ok, {Lvl, Wins}} ->
              logger:notice("User ~p lost a game", [User]),
              New = lists:min([-1, Wins - 1]),
              login_service(maps:update({User, Pass}, calc(Lvl, New), Data), Usernames, SessionManager);
            error ->
              login_service(Data, Usernames, SessionManager)
          end;
        error ->
          login_service(Data, Usernames, SessionManager)
      end;
    {Pid, draw} ->
      case maps:find(Pid, SessionManager) of
        {ok, {User, Pass}} ->
          case maps:find({User, Pass}, Data) of
            {ok, {Lvl, _Wins}} ->
              login_service(maps:update({User, Pass}, {Lvl, 0}, Data), Usernames, SessionManager);
            error ->
              login_service(Data, Usernames, SessionManager)
          end;
        error ->
          login_service(Data, Usernames, SessionManager)
      end;
    {Pid, top, N} ->
      Sorted = lists:sort(fun({_, A}, {_, B}) -> A > B end, maps:to_list(Data)),
      TopN = lists:sublist(Sorted, N),
      Result = lists:map(fun({{User, _Pass}, {Lvl, Wins}}) -> {User, Lvl, Wins} end, TopN),
      Pid ! {self(), top, Result},
      login_service(Data, Usernames, SessionManager);
    {_Pid, save} ->
      save_to_file({Data, Usernames}),
      logger:notice("Backup successful"),
      login_service(Data, Usernames, SessionManager);
    {_Pid, shutdown} ->
      save_to_file({Data, Usernames})
  end.




