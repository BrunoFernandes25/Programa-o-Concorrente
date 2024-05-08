class Barrier {
    public class Etapa {}

    private int N;
    private int counter = 0;
    private int retorno = 0;
    private Etapa current;


    Barrier (int N) {
        this.N = N;
    }

    //Exercício 1 *
    public synchronized void await() throws InterruptedException {
        counter+= 1;

        if(counter == N) {
            notifyAll(); //acorda todas as Threads que tenham feito wait, é como um signal visto em aula
            counter = 0;
            retorno+=1;                                                                        
        } else {
            while(counter<N && retorno == 0){
                wait();
            }
        }
    }
    // Exercicio 2 -> solucao do stor
    /*public synchronized void await2() throws InterruptedException {
        counter +=1;
        int r = retorno; //isto garante que tou na etapa/retorno x se incrementar, ou seja cheguei ao fim entao nao faço wait
        if(counter == N){
            notifyAll();
            retorno+=1; //etapa que ele falava
            counter = 0;
        }
        else{
            while(counter<N && retorno == r ){
                wait();
            }
        }
    }*/

    public synchronized void await2() throws InterruptedException {
        Etapa e = current; //isto garante que tou na etapa/retorno x se incrementar, ou seja cheguei ao fim entao nao faço wait
        counter +=1;
        if(counter == N){
            notifyAll();
            //etapa +=1; //etapa que ele falava
            current = new Etapa();
            counter = 0;
        }
        else{
            while(counter<N && current == e ){
                wait();
            }
        }
    }
}
//colocamos syncronized no método pois nao há nada de calculo que precisasse de syncronized em algo mais restrito

 //Exercício 3 (feito em casa)
 class Agreement {
    public class etapa {
        public int max;

        public etapa() {
            this.max = 0;
        }

    }

    private int N;
    private int counter;
    private etapa current;

    Agreement (int N) {
        this.N = N;
        this.counter = 0;
        this.current = new etapa();
    }

    synchronized int propose(int choice) throws InterruptedException {
        counter++;
        etapa e = current;
        if(choice > e.max)
            e.max = choice;

        if(counter == N) {
            counter = 0;
            this.current = new etapa();
            notifyAll();
        } else {
            while(e == current) wait();
        }
        return e.max;
    }
}


public class Main {
    public static void main(String[] args) {
        // Exercício 1: Barrier
        Barrier barrier = new Barrier(5); // Criar uma barreira para 5 threads
        for (int i = 0; i < 5; i++) {
            Thread thread = new Thread(() -> {
                try {
                    System.out.println("Thread started");
                    barrier.await(); // Espera na barreira
                    System.out.println("Thread finished");
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            });
            thread.start();
        }

        // Exercício 2: Barrier (reutilizável)
        Barrier reusableBarrier = new Barrier(7); // Criar uma barreira reutilizável para 3 threads
        for (int i = 0; i < 7; i++) {
            Thread thread = new Thread(() -> {
                try {
                    System.out.println("Reusable Thread started");
                    reusableBarrier.await2(); // Espera na barreira
                    System.out.println("Reusable Thread finished");
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            });
            thread.start();
        }

        // Exercício 3: Agreement
        Agreement agreement = new Agreement(4); // Criar um acordo para 4 threads
        for (int i = 0; i < 4; i++) {
            final int proposal = i * 10; // Propor valores diferentes para cada thread
            Thread thread = new Thread(() -> {
                try {
                    System.out.println("Thread proposing: " + proposal);
                    int agreedValue = agreement.propose(proposal); // Propor um valor
                    System.out.println("Agreed value: " + agreedValue);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            });
            thread.start();
        }
    }
}

/* 
[ou para o exercicio 1]

public synchronized void await() throws InterruptedException {
        counter+= 1;
        while(counter<N){
            wait();
        }
        notify(); //fazemos apenas um notify e cada uma acorda a próxima, com notifyAll() iriam todas notifyAll e nao daria asneira mas nao era tao correto
    }
*/