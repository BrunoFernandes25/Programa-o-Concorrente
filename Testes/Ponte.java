import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;


class Ponte {
    int ocupacao = 0;
    Lock l = new ReentrantLock();
    Condition travessiaPossivel = l.newCondition();

    void inicioTravessiaIda() throws InterruptedException {
        l.lock();
        try {
            while (ocupacao == 10) {
                System.out.println("\nPonte cheia, esperando...");
                travessiaPossivel.await();
            }
            ocupacao++;
            System.out.println("Início travessia ida. Ocupação: " + ocupacao);
        } finally {
            l.unlock();
        }
    }

    void inicioTravessiaVolta() throws InterruptedException {
        l.lock();
        try {
            while (ocupacao == 10) {
                System.out.println("Ponte cheia, esperando...");
                travessiaPossivel.await();
            }
            ocupacao++;
            System.out.println("Início travessia volta. Ocupação: " + ocupacao);
        } finally {
            l.unlock();
        }
    }

    void fimTravessia() {
        l.lock();
        try {
            ocupacao--;
            System.out.println("Fim travessia. Ocupação: " + ocupacao);
            travessiaPossivel.signalAll();
        } finally {
            l.unlock();
        }
    }

    public static void main(String[] args) {
        Ponte ponte = new Ponte();
    
        // Exemplo de uso com threads
        Runnable ida = () -> {
            try {
                ponte.inicioTravessiaIda();
                Thread.sleep(1000); // Simula o tempo de travessia
                ponte.fimTravessia();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        };
    
        Runnable volta = () -> {
            try {
                ponte.inicioTravessiaVolta();
                Thread.sleep(1000); // Simula o tempo de travessia
                ponte.fimTravessia();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        };
    
        for (int i = 0; i < 12; i++) {
            new Thread(ida).start();
            new Thread(volta).start();
        }
        System.out.println("Todas as travessias foram concluídas.");
    }
}
