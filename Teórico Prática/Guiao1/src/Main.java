/*class MyThread extends Thread{
    public void run(){
        for(long i = 0;i<10000000000L;i++);
        System.out.println("Na Thread");
    }
}

class MyRunnable implements Runnable{
    public void run(){
        for(long i = 0;i<10000000000L;i++);
        System.out.println("No Runnable");
    }
}

public class Main {
    public static void main(String[] args) {
        MyThread t1 = new MyThread();
        Thread t2 = new Thread(new MyRunnable());
        t1.start();
        t2.start();

        System.out.println("No Main");
        //for (long i = 0;i<10000000000L;i++); //aumentando 1 zero demora bem mais na execução
        //t.run();
    }

}*/

//run() -> When an object implementing interface Runnable is used to create a thread, starting the thread causes the object's run method to be called in that separately executing thread.
//start() -> Causes this thread to begin execution; the Java Virtual Machine calls the run method of this thread.
// join() -> Waits for this thread to die
//sleep(1000) -> Causes the currently executing thread to sleep (temporarily cease execution) for the specified number of milliseconds, subject to the precision and accuracy of system timers and schedulers.


//RESOLUÇÃO GUIÃO 1

//Exercício 1
class Printer extends Thread {

    int I;

    public Printer(int I){
        this.I = I;
    }

    public void run(){
        for(int i =0;i<I;i++){
            System.out.println(i);
        }
    }
}

//Main exercício 1
public class Main {
    public static void main(String[] args) throws InterruptedException {  //com throws evitamos try catchs mas só porque tamos na main
        int N = Integer.parseInt(args[0]);      //nº Threads
        int I = Integer.parseInt(args[1]);      //nº Incrementos

        //criando array de Threads
        Printer[] a = new Printer[N];
        for(int i = 0;i<N;i++){
            a[i] = new Printer(I);
            //ou
            //a[i] = new Printer();
            //a[i].I = I;
            //e nao criavamos o public Printer(int I){this.I = I;}
        }
        for(int i = 0;i<N;i++){
            a[i].start();
        }
        for(int i = 0;i<N;i++){     //sem o join, FIM já nao apareceria no fim, podendo aparecer no meio ou até no inicio
            a[i].join();
        }    

        System.out.println("FIM exercício 1");
    }
}


//Exercício 2/3

class Counterv2{  //counter vai ser partilhado por Threads logo nao extendo das Threads
    int i = 0;      //apenas contem 1 inteiro para o exercicio 2
}

class Counterv3{
    private int i = 0;
    void inc(){i=i+1;}
    int value(){return i;}
}

class Incrementer extends Thread{
    final int I;
    final Counterv3 c;
    public Incrementer (int I,Counterv3 c){this.I= I;this.c = c;}
    public void run() {
        //aqui nao podia criar um Counter senao nao ia ser partilhado pelas threads mas sim todas elas iam criar 1 counter
        for (long i = 0; i < I; i++)
            c.inc();
    }

}
//Main exercício 2
public class Main {
    public static void main(String[] args) throws InterruptedException {  //com throws evitamos try catchs mas só porque tamos na main
        int N = Integer.parseInt(args[0]);      //nº Threads
        int I = Integer.parseInt(args[1]);      //nº Incrementos


        //criando array de Threads
        Incrementer[] a = new Incrementer[N];
        Counterv3 c = new Counterv3();

        for(int i = 0;i<N;i++){
            a[i] = new Incrementer(I,c);
        }
        for(int i = 0;i<N;i++){
            a[i].start();
        }
        for(int i = 0;i<N;i++){
            a[i].join();
        }

        System.out.println(c.value());
        System.out.println("FIM exercício 2");
    }
}

//Mas desta forma para numeros de incrementos maiores as Threads nao conseguem retornar o valor correto Main 2 500 dá 487 ou 493 (Solucao guiao 2 ex1 no ficheiro Main.java)