import java.util.HashMap;

class Votacao {

    private final HashMap<String, Integer> votos = new HashMap<>();
    private boolean condicaoSatisfeita = false;

    public synchronized void vota(String candidato) {
        votos.put(candidato, votos.getOrDefault(candidato, 0) + 1);
        //System.out.println("Voto registrado para " + candidato + ". Total: " + votos.get(candidato));
        if(!condicaoSatisfeita) notifyAll();
    }

    public synchronized void espera(String c1, String c2, String c3) throws InterruptedException {
        while (!condicaoSatisfeita) {
            int cand1 = votos.getOrDefault(c1, 0);
            int cand2 = votos.getOrDefault(c2, 0);
            int cand3 = votos.getOrDefault(c3, 0);

            if (cand1 < cand2 && cand2 < cand3) {
                condicaoSatisfeita = true;
                //System.out.println("Condição satisfeita: V(" + c1 + ") < V(" + c2 + ") < V(" + c3 + ")");
            } else {
                //System.out.println("Esperando... V(" + c1 + ")=" + cand1 + " V(" + c2 + ")=" + cand2 + " V(" + c3 + ")=" + cand3);
                wait(); // Aguarda até que a condição seja satisfeita
            }
        }
    }

    public static void main(String[] args) {
        Votacao votacao = new Votacao();

        // Exemplo de uso com threads
        Runnable votarC1 = () -> {
            for (int i = 0; i < 10; i++) {
                votacao.vota("c1");
                try {
                    Thread.sleep(100); // Simula tempo entre votos
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        };

        Runnable votarC2 = () -> {
            for (int i = 0; i < 15; i++) {
                votacao.vota("c2");
                try {
                    Thread.sleep(100); // Simula tempo entre votos
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        };

        Runnable votarC3 = () -> {
            for (int i = 0; i < 20; i++) {
                votacao.vota("c3");
                try {
                    Thread.sleep(100); // Simula tempo entre votos
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        };

        Runnable esperar = () -> {
            try {
                votacao.espera("c1", "c2", "c3");
                System.out.println("Condição de espera satisfeita: V(c1) < V(c2) < V(c3)");
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        };

        // Cria e inicia threads de exemplo
        Thread t1 = new Thread(votarC1);
        Thread t2 = new Thread(votarC2);
        Thread t3 = new Thread(votarC3);
        Thread t4 = new Thread(esperar);

        t1.start();
        t2.start();
        t3.start();
        t4.start();

        try {
            t1.join();
            t2.join();
            t3.join();
            t4.join();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
}
