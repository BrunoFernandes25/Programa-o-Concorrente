import java.util.HashMap;

class Votacao {

    HashMap <String,Integer> votos = new HashMap<String,Integer>();

    synchronized void espera(String c1,String c2,String c3) throws InterruptedException {
        int cand1 = votos.get(c1);
        int cand2 = votos.get(c2);
        int cand3 = votos.get(c3);

        if(cand1 < cand2 && cand2 < cand3){
            notifyAll();
        }
        else{
            while(cand1 > cand2 || cand2 > cand3){
                wait();
            }
        }

    }

    synchronized void vota(String candidato) {
        if (votos.containsKey(candidato)) {
            votos.put(candidato, votos.get(candidato) + 1);
        } else {
            votos.put(candidato, 1);
        }
    }
}

public class Votos {
    public static void main(String[] args) {
        Votacao votacao = new Votacao();

        // Votações
        votacao.vota("candidato1");
        votacao.vota("candidato2");
        votacao.vota("candidato3");
        votacao.vota("candidato1");
        votacao.vota("candidato2");
        votacao.vota("candidato2");
        votacao.vota("candidato3");
        votacao.vota("candidato3");
        votacao.vota("candidato3");

        // Thread 1 - Verificação
        Thread thread1 = new Thread(() -> {
            try {
                System.out.println("Thread 1: Esperando sequência crescente...");
                votacao.espera("candidato1", "candidato2", "candidato3");
                System.out.println("Thread 1: Sequência crescente alcançada!");
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        });

        // Thread 2 - Nova votação
        Thread thread2 = new Thread(() -> {
            try {
                Thread.sleep(2000); // Espera um pouco antes de realizar outra votação
                System.out.println("Thread 2: Realizando nova votação...");
                votacao.vota("candidato1");
                votacao.vota("candidato2");
                votacao.vota("candidato3");
                System.out.println("Thread 2: Votação realizada!");
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        });

        // Inicia as threads
        thread1.start();
        thread2.start();

        // Aguarda o término das threads
        try {
            thread1.join();
            thread2.join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        System.out.println("Todas as threads foram concluídas.");
    }
}
