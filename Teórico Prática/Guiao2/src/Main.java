class Counterv2{  //counter vai ser partilhado por Threads logo nao extendo das Threads
    int i = 0;      //apenas contem 1 inteiro para o exercicio 2
}

class Counterv3{
    private int i = 0;
    synchronized void inc(){i=i+1;}       //SOLUÇÃO
    synchronized int value(){return i;}  //SOLUÇÃO
}
/*
class Incrementer2 implements Runnable{
    final int I;
    final Counterv3 c;
    public Incrementer2 (int I,Counterv3 c){this.I= I;this.c = c;}
    public void run() {
        //aqui nao podia criar um Counter senao nao ia ser partilhado pelas threads mas sim todas elas iam criar 1 counter
        for (long i = 0; i < I; i++)
            c.inc();
    }
}*/

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
    }
}