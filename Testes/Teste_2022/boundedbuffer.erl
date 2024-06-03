-module(boundedbuffer).
-export([start/0,waitForConsumer/1,waitForProducer/1]).

start() -> spawn(fun()-> matcher([],[]) end).

waitForConsumer(Matcher)->
  Matcher ! {waitConsumer, self()},
  receive
    {BB,Matcher} -> BB
  end.

waitForProducer(Matcher)->
  Matcher ! {waitProducer, self()},
  receive
    {BB,Matcher} -> BB
  end.

matcher([Prod| Prods],[Con| Cons]) ->
  BB = newBoundedBuffer:new(),
  Prod ! {BB,self()},
  Con ! {BB,self()},
  matcher(Prods,Cons);

matcher(Prods,Cons)->
  receive
    {waitForConsumer,From} ->
      matcher(Prods ++ [From],Cons);
    {waitForProducer,From} ->
      matcher(Prods,Cons ++ [From])
  end.
