import java.util.concurrent.locks.*;

class Evento {
    Lock l =new ReentrantLock();
    Condition evento_tipo = l.newCondition();

    final int tipo1 = 1;
    final int tipo2 = 2;
    private int n1; //private para cada evento poder ter acesso e nao qualquer evento
    private int n2;

    //Criamos o construtor desdta forma para poder incrementar n1 e n2 a cada evento
    Evento(int n1, int n2) {
        this.n1 = n1;
        this.n2 = n2;
    }

    void espera(int tipo1, int n1, int tipo2, int n2) throws InterruptedException {
        l.lock();
        try{
            while( this.n1 < n1 || this.n2 < n2){
                evento_tipo.await();
            }
        }
        finally {
            l.unlock();
        }
    }

    void sinaliza(int tipo){
        l.lock();
        try{
            if(tipo == this.tipo1 && this.n1 < n1){
                this.n1++;
            }
            if(tipo == this.tipo2 && this.n2 < n2){
                this.n2++;
            }
            evento_tipo.signalAll();
        }
        finally {
            l.unlock();
        }
    }
}

class Eventos extends Thread {
    public static void main(String[] args) throws InterruptedException {

        Evento evts = new Evento(4,3);

        Thread threadEspera = new Thread (() -> {
            try{
                evts.espera(1, 4, 2, 3);
            } catch (Exception e) { }
        });


        Thread threadSinaliza = new Thread(() -> {
            try {
                System.out.println("vou sinalizar 1 1x");
                evts.sinaliza(1);
                Thread.sleep(100);

                System.out.println("vou sinalizar 2 1x");
                evts.sinaliza(2);

                System.out.println("vou sinalizar 1 2x");
                evts.sinaliza(1);
                Thread.sleep(100);

                System.out.println("vou sinalizar 1 3x");
                evts.sinaliza(1);
                Thread.sleep(100);

                System.out.println("vou sinalizar 2 2x");
                evts.sinaliza(2);
                Thread.sleep(100);

                System.out.println("vou sinalizar 1 4x");
                evts.sinaliza(1);
                Thread.sleep(100);

                System.out.println("vou sinalizar 2 3x");
                evts.sinaliza(2);
                Thread.sleep(100);

            } catch (Exception e) {
                e.printStackTrace();
            }
        });

        // Iniciar as threads
        threadEspera.start();
        threadSinaliza.start();

        // Aguardar término das threads
        threadEspera.join();
        threadSinaliza.join();
    }

}
