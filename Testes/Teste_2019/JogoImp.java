import java.util.Random;

import static java.lang.Thread.sleep;

interface Jogo {
    Partida participa() throws InterruptedException;
}

interface Partida {
    String adivinha(int n) throws InterruptedException;
}

class JogoImpl implements Jogo {
    private PartidaImpl p = new PartidaImpl();
    private int jogadores = 0;

    public synchronized Partida participa() throws InterruptedException {
        jogadores++;
        PartidaImpl atual = p;

        if(jogadores == 4) {
            notifyAll();
            jogadores = 0;
            p = new PartidaImpl();     //nova partida para os proximos

            new Thread(() -> {              //conta os 60 segundos ate disparar
                try {
                    sleep(6000);
                } catch (InterruptedException e) {
                    p.timeout();
                }
            }).start();
        }
        else{
            while(p == atual) wait();
        }
        return atual;
    }
}

class PartidaImpl implements Partida {
    int tentativas = 0;
    int numero = new Random().nextInt(100);
    boolean ganhou = false;
    boolean timeout = false;

    synchronized void timeout() {
        timeout = true;
    }

    public synchronized String adivinha(int n) {
        tentativas += 1;

        if(ganhou) return "PERDEU";
        if(timeout) return "TEMPO";
        if(tentativas > 100) return "TENTATIVAS";
        if(n == numero) {
            ganhou = true;
            return "GANHOU";
        }
        if(numero < n) return "MENOR";
        return "MAIOR";
    }

}
