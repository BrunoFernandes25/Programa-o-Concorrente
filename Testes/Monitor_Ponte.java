import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

//COM SIGNALS
class Ponte {
    int ocupacao = 0;
    Lock l = new ReentrantLock();
    Condition travessiaPossivel = l.newCondition();

    void inicioTravessiaIda() throws InterruptedException {
        l.lock();
        try {
            while (ocupacao == 10) {
                System.out.println("\nPonte cheia\n");
                travessiaPossivel.await();
            }
            ocupacao++;
            Thread.sleep(500);
        } finally {
            l.unlock();
        }
    }

    void inicioTravessiaVolta() throws InterruptedException {
        l.lock();
        try {
            while (ocupacao == 10) {
                System.out.println("\nPonte cheia\n");
                travessiaPossivel.await();
            }
            ocupacao++;
            Thread.sleep(500);
        } finally {
            l.unlock();
        }
    }

    void fimTravessia() {
        l.lock();
        try {
            ocupacao--;
            travessiaPossivel.signalAll();
        } finally {
            l.unlock();
        }
    }
}


/*
class Ponte {
    int ocupacao = 0;
    Lock l = new ReentrantLock();

    void inicioTravessiaIda() throws InterruptedException {
        l.lock();
        try {
            while (ocupacao == 10) {
                System.out.println("\nPonte cheia\n");
                wait();
            }
            ocupacao++;
            Thread.sleep(500);
        } finally {
            l.unlock();
        }
    }

    void inicioTravessiaVolta() throws InterruptedException {
        l.lock();
        try {
            while (ocupacao == 10) {
                System.out.println("\nPonte cheia\n");
                wait();
            }
            ocupacao++;
            Thread.sleep(500);
        } finally {
            l.unlock();
        }
    }

    synchronized void fimTravessia() {
            ocupacao--;
            notifyAll();
    }
}*/

public class Monitor_Ponte {
    public static void main(String[] args) {
        Ponte ponte = new Ponte(); // Instância única de Ponte para todas as threads
        Thread[] threads = new Thread[30]; // Array para armazenar as threads

        // Cria e inicia múltiplas threads para simular a travessia
        for (int i = 0; i < threads.length; i++) {
            final int index = i; // Índice da thread atual

            threads[i] = new Thread(() -> {
                try {
                    if (index % 2 != 0) {
                        ponte.inicioTravessiaIda();
                        System.out.println("Travessia de ida iniciada por thread " + Thread.currentThread().getId());
                    } else {
                        ponte.inicioTravessiaVolta();
                        System.out.println("Travessia de volta iniciada por thread " + Thread.currentThread().getId());
                    }

                    Thread.sleep(500); // Simulando travessia
                    ponte.fimTravessia();
                    System.out.println("Travessia finalizada por thread " + Thread.currentThread().getId());
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            });

            // Inicia a thread
            threads[i].start();
        }

        // Aguarda o término de todas as threads
        for (Thread thread : threads) {
            try {
                thread.join();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        System.out.println("Todas as travessias foram concluídas.");
    }
}