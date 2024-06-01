import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

interface Controller {
    int request_resource(int i) throws InterruptedException;
    void release_resource(int i);
}

class ResourceController implements Controller {
    Lock lock = new ReentrantLock();
    Condition Rec0 = lock.newCondition();
    Condition Rec1 = lock.newCondition();
    private int T, counter = 0, turno = 0;

    ResourceController(int T) {
        this.T = T;
    }

    public int request_resource(int i) throws InterruptedException {
        lock.lock();
        try {
            if (i == 0) {
                while (turno == 1 || counter == T) Rec0.await();
            } else if (i == 1) {
                while (turno == 0 || counter == T) Rec1.await();
            }
            counter++;
            return 1;
        } finally {
            lock.unlock();
        }
    }

    public void release_resource(int i) {
        lock.lock();
        try {
            counter--;
            if (i == 1) {
                if (counter == 0) {
                    turno = 0;
                    Rec0.signalAll();
                } else Rec1.signalAll();
            }
            if (i == 0) {
                if (counter == 0) {
                    turno = 1;
                    Rec1.signalAll();
                } else Rec0.signalAll();
            }
        } finally {
            lock.unlock();
        }
    }
}

public class Controlo {
    public static void main(String[] args) {
        final int T = 5; // Defina o número máximo de threads permitidas acessando um recurso simultaneamente

        ResourceController controller = new ResourceController(T);

        // Exemplo de uso das threads
        for (int i = 0; i < 20; i++) {
            int resourceId = i % 2; // Alternando entre os recursos disponíveis
            Thread thread = new Thread(() -> {
                try {
                    int resource = controller.request_resource(resourceId);
                    if (resource != -1) {
                        System.out.println("Thread " + Thread.currentThread().getId() + " accessed resource " + resource);
                        // Simula o uso do recurso
                        Thread.sleep(500); // Simula o uso do recurso
                        controller.release_resource(resourceId);
                    } else {
                        System.out.println("Thread " + Thread.currentThread().getId() + " failed to acquire resource");
                    }
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            });
            thread.start();
        }
    }
}