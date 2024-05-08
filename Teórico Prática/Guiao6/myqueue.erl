-module(myqueue).
-export([create/0,enqueue/2,dequeue/1,test/0]).

% create() -> [].

% enqueue(Queue, Item) -> Queue ++ [Item]. %desta fomma se tivermos 1000 enqueues iremos ter 100o copias tempo(N) de execução

% dequeue([]) -> empty;
% dequeue([Item|Queue]) -> {Queue, Item}.  % fizemos desta forma retornando a tail(Queue de [Item|Queue]) e colocamos o Item a remover {..,Item}

% enqueue mais eficiente:

%create() -> Queue
create() -> {[],[]}.    %inicialização das 2 filas a utilizar fila In e Out respetivamente

%enqueue(Queue, Item) -> Queue
enqueue({In, Out}, Item) -> {[Item | In], Out}.   %inserção à cabeça da fila In

%dequeue(Queue) -> empty | {Queue, Item}
dequeue({[], []}) -> empty;
dequeue({In,[Item | Tail]}) -> {{In,Tail}, Item};     % caso haja items na lista Out removemos o item simplesmente ficando com a cauda(Tail) 
dequeue({In,[]}) -> dequeue({[],lists:reverse(In)}).  % caso Out esteja vazio temos que retirar da lista In o item mais antigo que está na parte de baixo da pilha logo temos que inverter a lista In colocar em Out e remover o Item 

test() -> 
    Q1 = create(),
    empty = dequeue(Q1),
    Q2 = enqueue(Q1,1),
    Q3 = enqueue(Q2,2),
    {_,1} = dequeue(Q2),
    {Q4,1} = dequeue(Q3),
    {Q5,2} = dequeue(Q4),
    empty = dequeue(Q5),
    ok.

% se falhar test entao crasho
