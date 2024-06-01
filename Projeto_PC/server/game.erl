-module(game).
-export([start/1]).

-define(BORDER_X, 1280).
-define(BORDER_Y, 720).
-define(UPDATE_INTERVAL, 15).
-define(SUN_X, 640.0).
-define(SUN_Y, 360.0).
-define(SUN_RADIUS, 50.0).
-define(SUN_STRENGTH, 0.2).
-define(ACCEL, 0.5).
-define(FUEL_COST, 1.0).
-define(ANGLE_ACCEL, 0.003).
-define(PLAYER_RADIUS, 15.0).

randRange(Min, Max) ->
  rand:uniform() * (Max - Min) + Min.

genPlanet(Low, High, Id) ->
  Radius = randRange(20, 30),
  Distance = randRange(Low, High) + ?SUN_RADIUS,
  Angle = randRange(0, 2 * math:pi()),
  Speed = randRange(0.01, 0.03),
  X = Distance * math:cos(Angle) + ?SUN_X,
  Y = Distance * math:sin(Angle) + ?SUN_Y,
  {X, Y, Radius, Distance, Angle, Speed, Id}.

genPlanets() ->
  [
    genPlanet(50, 70, -1),
    genPlanet(130, 150, -2),
    genPlanet(200, 220, -3)
  ].

movePlanet({_, _, Radius, Distance, Angle, Speed, Id}) ->
  NewAngle = Angle + Speed,
  NewX = Distance * math:cos(NewAngle) + ?SUN_X,
  NewY = Distance * math:sin(NewAngle) + ?SUN_Y,
  {NewX, NewY, Radius, Distance, NewAngle, Speed, Id}.

intersect({X1, Y1, R1}, {X2, Y2, R2}) ->
  math:sqrt((X1 - X2) * (X1 - X2) + (Y1 - Y2) * (Y1 - Y2)) < (R1 + R2).

collisionTest({X, Y, _, _, _, _, _}, Planets) ->
  OutsideBorder = (X > ?BORDER_X) or (X < 0) or (Y > ?BORDER_Y) or (Y < 0),
  SunIntersect = intersect({X, Y, ?PLAYER_RADIUS}, {?SUN_X, ?SUN_Y, ?SUN_RADIUS}),
  PlanetIntersect = lists:any(fun({I, J, R, _, _, _, _}) ->
    intersect({X, Y, ?PLAYER_RADIUS}, {I, J, R}) end, Planets),
  SunIntersect or PlanetIntersect or OutsideBorder.

elasticCollision({X, Y, AngleSpeed, Angle, Speed, Fuel, Id}, {X2, Y2, _AngleSpeed2, Angle2, Speed2, _Fuel2, Id2}) ->
  Test = intersect({X, Y, ?PLAYER_RADIUS}, {X2, Y2, ?PLAYER_RADIUS}),
  if
    Id == Id2  ->
      {X, Y, AngleSpeed, Angle, Speed, Fuel, Id};
    Test ->
      {X, Y, AngleSpeed, Angle + math:fmod(abs(Angle - Angle2), 2*math:pi()), Speed2, Fuel, Id};
    true ->
      {X, Y, AngleSpeed, Angle, Speed, Fuel, Id}
  end.

collisionTestPlayers(P1, Players) ->
  maps:fold(fun(_, P2, Acc) -> elasticCollision(Acc, P2) end, P1, Players).

movePlayer({X, Y, AngleSpeed, Angle, Speed, Fuel, Id}) ->
  if
    (X > ?SUN_X) and (Y > ?SUN_Y) ->
      {X + Speed * math:cos(Angle) - ?SUN_STRENGTH, Y + Speed * math:sin(Angle) - ?SUN_STRENGTH, AngleSpeed, Angle + AngleSpeed, Speed, Fuel, Id};
    (X < ?SUN_X) and (Y > ?SUN_Y) ->
      {X + Speed * math:cos(Angle) + ?SUN_STRENGTH, Y + Speed * math:sin(Angle) - ?SUN_STRENGTH, AngleSpeed, Angle + AngleSpeed, Speed, Fuel, Id};
    (X < ?SUN_X) and (Y < ?SUN_Y) ->
      {X + Speed * math:cos(Angle) + ?SUN_STRENGTH, Y + Speed * math:sin(Angle) + ?SUN_STRENGTH, AngleSpeed, Angle + AngleSpeed, Speed, Fuel, Id};
    true ->
      {X + Speed * math:cos(Angle) - ?SUN_STRENGTH, Y + Speed * math:sin(Angle) + ?SUN_STRENGTH, AngleSpeed, Angle + AngleSpeed, Speed, Fuel, Id}
  end.

movePlayer({X, Y, AngleSpeed, Angle, Speed, Fuel, Id}, _Action) when Fuel < ?FUEL_COST ->
  {X, Y, AngleSpeed, Angle, Speed, Fuel, Id};

movePlayer({X, Y, AngleSpeed, Angle, Speed, Fuel, Id}, up) ->
  {X, Y, AngleSpeed, Angle, Speed + ?ACCEL, Fuel - ?FUEL_COST, Id};

movePlayer({X, Y, AngleSpeed, Angle, Speed, Fuel, Id}, left) ->
  {X, Y, AngleSpeed + ?ANGLE_ACCEL, Angle, Speed, Fuel - ?FUEL_COST, Id};

movePlayer({X, Y, AngleSpeed, Angle, Speed, Fuel, Id}, right) ->
  {X, Y, AngleSpeed - ?ANGLE_ACCEL, Angle, Speed, Fuel - ?FUEL_COST, Id}.

genPlayer(Id) ->
  {X, Y} =
    case Id rem 4 of
             0 -> {randRange(100.0, 300.0), randRange(100.0, 300.0)};
             1 -> {randRange(?BORDER_X - 400, ?BORDER_X - 200), randRange(?BORDER_Y - 250, ?BORDER_Y - 50)};
             2 -> {randRange(100.0, 300.0), randRange(?BORDER_Y - 200, ?BORDER_Y - 50)};
             3 -> {randRange(?BORDER_X - 400, ?BORDER_X - 200), randRange(100.0, 300.0)}
    end,
  {
    X,
    Y,
    0.0,
    0.0,
    0.0,
    100.0,
    Id
  }.

start(Players) ->
  logger:notice("~p players matched", [length(Players)]),
  timer:send_interval(?UPDATE_INTERVAL, update),
  PlayerObjects = maps:from_list(lists:map(fun({Index, X}) -> {X, genPlayer(Index)} end, lists:enumerate(Players))),
  Planets = genPlanets(),
  lists:foreach(fun(X) ->
    {_, _, _, _, _, _, Id} = maps:get(X, PlayerObjects),
    X ! {self(), start, Id} end, Players),
  Objects = buildUpdate(Planets, PlayerObjects),
  lists:foreach(fun(X) -> X ! {self(), update, Objects} end, Players),
  game(Players, Planets, [], PlayerObjects).

buildUpdate(Planets, PlayerObjects) ->
  PlayerObjectsValues = lists:map(fun({_, {X, Y, _, Angle, _, _, Id}}) ->
    {X, Y, ?PLAYER_RADIUS, Angle, Id} end, maps:to_list(PlayerObjects)),
  PlanetsValues = lists:map(fun({X, Y, Radius, _, Angle, _, Id}) ->
    {X, Y, Radius, Angle, Id} end, Planets),
  Sun = {?SUN_X, ?SUN_Y, ?SUN_RADIUS, 0.0, 0},
  [Sun | (PlayerObjectsValues ++ PlanetsValues)].

game([], _, PlayersLost, _) ->
  lists:foreach(fun(X) -> X ! {self(), draw} end, PlayersLost);

game(Players, Planets, PlayersLost, PlayerObjects) ->
  receive
    {Pid, leave} ->
      NewPlayers = lists:delete(Pid, Players),
      if
        (length(NewPlayers) == 1) and (length(Players) > 1) ->
          timer:send_after(5000, finish),
          game(NewPlayers, Planets, lists:delete(Pid, PlayersLost), maps:remove(Pid, PlayerObjects));
        true ->
          game(NewPlayers, Planets, lists:delete(Pid, PlayersLost), maps:remove(Pid, PlayerObjects))
      end;
    update ->
      NewPlayerObjects = maps:map(fun(_Key, Value) -> movePlayer(Value) end, PlayerObjects),
      NewPlanets = lists:map(fun movePlanet/1, Planets),
      {NewPlayersLost, NewPlayers} = lists:partition(fun(X) ->
        collisionTest(maps:get(X, NewPlayerObjects), NewPlanets) end, Players),
      lists:foreach(fun(X) -> X ! {self(), fuel, -1.0} end, NewPlayersLost),
      NewNewPlayerObjects = maps:without(NewPlayersLost, NewPlayerObjects),
      NewNewNewPlayerObjects = maps:map(fun(_, X) -> collisionTestPlayers(X, NewNewPlayerObjects) end, NewNewPlayerObjects),
      Objects = buildUpdate(NewPlanets, NewNewNewPlayerObjects),
      lists:foreach(fun(X) -> X ! {self(), update, Objects} end, Players),
      lists:foreach(fun(X) -> X ! {self(), update, Objects} end, PlayersLost),
      if
        (length(NewPlayers) == 1) and (length(Players) > 1) ->
          timer:send_after(5000, finish),
          game(NewPlayers, NewPlanets, PlayersLost ++ NewPlayersLost, NewNewNewPlayerObjects);
        true ->
          game(NewPlayers, NewPlanets, PlayersLost ++ NewPlayersLost, NewNewNewPlayerObjects)
      end;
    finish ->
      lists:foreach(fun(X) -> X ! {self(), win} end, Players),
      lists:foreach(fun(X) -> X ! {self(), loss} end, PlayersLost);
    {Pid, Action} ->
      case maps:find(Pid, PlayerObjects) of
        {ok, Val} ->
          {_, _, _, _, _, PrevFuel, _} = Val,
          NewVal = movePlayer(Val, Action),
          if
            (PrevFuel >= ?FUEL_COST) ->
              {_, _, _, _, _, Fuel, _} = NewVal,
              Pid ! {self(), fuel, Fuel};
            true -> ok
          end,
          game(Players, Planets, PlayersLost, maps:update(Pid, NewVal, PlayerObjects));
        error ->
          game(Players, Planets, PlayersLost, PlayerObjects)
      end
  end.