-module(matchmaking).
-export([start/0]).
-define(PLAYER_LIMIT, 4).
-define(WAIT_TIME, 5000).

start() ->
  Pid = spawn(fun() -> timing_service() end),
  matchmaking_service(queue:new(), Pid).

matchmaking_service(Queue, UpdatePid) ->
  receive
    {Pid, join, _Lvl} ->
      New = queue:in(Pid, Queue),
      Size = queue:len(New),
      if
        Size > (?PLAYER_LIMIT - 1) ->
          UpdatePid ! {self(), cancel},
          {Prev, New2} = queue:split(?PLAYER_LIMIT, New),
          List = queue:to_list(Prev),
          _ = spawn(game, start, [List]),
          matchmaking_service(New2, UpdatePid);
        Size > 1 ->
          UpdatePid ! {self(), reset, ?WAIT_TIME, Size},
          matchmaking_service(New, UpdatePid);
        true ->
          UpdatePid ! {self(), cancel},
          matchmaking_service(New, UpdatePid)
      end;

    {Pid, leave, _Lvl} ->
      New = queue:delete(Pid, Queue),
      Size = queue:len(New),
      Pid ! {self(), leave},
      if
        Size > 1 ->
          UpdatePid ! {self(), reset, ?WAIT_TIME, Size};
        true ->
          UpdatePid ! {self(), cancel}
      end,
      matchmaking_service(New, UpdatePid);

    {_Pid, timeout, Matching} ->
      Size = queue:len(Queue),
      if
        (Matching > Size) ->
          matchmaking_service(Queue, UpdatePid);
        true ->
          {Prev, New} = queue:split(Matching, Queue),
          List = queue:to_list(Prev),
          _ = spawn(game, start, [List]),
          matchmaking_service(New, UpdatePid)
      end
  end.

timing_service() ->
  receive
    {Pid, reset, Time, Data} ->
      {ok, Ref} = timer:send_after(Time, Pid, {self(), timeout, Data}),
      timing_service(Ref);
    {_Pid, cancel} ->
      timing_service()
  end.

timing_service(Action) ->
  receive
    {Pid, reset, Time, Data} ->
      timer:cancel(Action),
      {ok, Ref} = timer:send_after(Time, Pid, {self(), timeout, Data}),
      timing_service(Ref);
    {_Pid, cancel} ->
      timer:cancel(Action),
      timing_service()
  end.