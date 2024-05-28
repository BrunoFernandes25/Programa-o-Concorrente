import java.util.concurrent.locks.*;

class Evento {
    private final Lock l = new ReentrantLock();
    private final Condition evento_tipo = l.newCondition();

    private final int E; // Número total de tipos de eventos
    private final int[] contagem; // Array para contar cada tipo de evento

    // Construtor que inicializa a contagem de eventos e o número de tipos de eventos
    Evento(int E) {
        this.E = E;
        this.contagem = new int[E + 1]; // Índices de 1 a E
    }

    // Método espera que bloqueia até que os eventos tipo1 e tipo2 ocorram n1 e n2 vezes, respectivamente
    void espera(int tipo1, int n1, int tipo2, int n2) throws InterruptedException {
        l.lock();
        try {
            while (contagem[tipo1] < n1 || contagem[tipo2] < n2) {
                evento_tipo.await();
            }
        } finally {
            l.unlock();
        }
    }

    // Método sinaliza que incrementa a contagem do tipo de evento e sinaliza todas as threads à espera
    void sinaliza(int tipo) {
        l.lock();
        try {
            if (tipo >= 1 && tipo <= E) {
                contagem[tipo]++;
                evento_tipo.signalAll();
            }
        } finally {
            l.unlock();
        }
    }
}

class TesteEventos extends Thread {
    public static void main(String[] args) throws InterruptedException {
        int E = 2; // Número de tipos de eventos
        Evento evts = new Evento(E);

        Thread threadEspera = new Thread(() -> {
            try {
                evts.espera(1, 4, 2, 3);
                System.out.println("Os eventos necessários foram sinalizados!");
            } catch (Exception e) {
                e.printStackTrace();
            }
        });

        Thread threadSinaliza = new Thread(() -> {
            try {
                System.out.println("vou sinalizar 1 1x");
                evts.sinaliza(1);
                Thread.sleep(100);

                System.out.println("vou sinalizar 2 1x");
                evts.sinaliza(2);
                Thread.sleep(100);

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
