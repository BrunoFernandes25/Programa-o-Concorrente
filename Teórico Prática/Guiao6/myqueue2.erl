-module(myqueue2).
-export([create/0,enqueue/3,dequeue/1,test/0]).

%create() -> PriQueue
create() -> [].

%enqueue(PriQueue, Item, Priority)
enqueue([],Item,Priority) -> [{Item,Priority}];
enqueue([{H,Hp} | Tail],Item,Priority) -> 
    if
        Priority > Hp ->
            [{Item,Priority} | [{H,Hp} | Tail]];
        true ->
            [{H,Hp} | enqueue(Tail,Item,Priority)]
    end.

%dequeue(PriQueue) -> empty | {PriQueue, Item}
dequeue([]) -> empty;
dequeue([{H,_}|T]) -> {T,H}.


test() ->
    % Testando a fila vazia
    empty = dequeue(create()),

    % Testando a inserção e remoção de um único elemento
    {_, 1} = dequeue(enqueue(create(), 1, 1)),

    % Testando a inserção e remoção de múltiplos elementos com prioridades diferentes
    {Q2, 1} = dequeue(enqueue(create(), 1, 1)),
    {Q3, 2} = dequeue(enqueue(Q2, 2, 2)),
    empty = dequeue(Q3),

    % Testando a inserção e remoção de múltiplos elementos com a mesma prioridade
    {Q4, 1} = dequeue(enqueue(create(), 1, 1)),
    {Q5, 2} = dequeue(enqueue(Q4, 2, 1)),
    empty = dequeue(Q5),
    ok.