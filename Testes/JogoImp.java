import java.util.Random;
import static java.lang.Thread.*;

interface Jogo {
    Partida participa() throws InterruptedException;
}

interface Partida {
    String adivinha(int n) throws InterruptedException;
}

class JogoImpl implements Jogo {
    private PartidaImpl partida = new PartidaImpl();
    private int jogadores = 0;

    public synchronized Partida participa() throws InterruptedException {
        jogadores++;
        PartidaImpl p = partida;

        if(jogadores == 4) {
            notifyAll();
            jogadores = 0;
            partida = new PartidaImpl();     //nova partida para os proximos

            new Thread(() -> {              //conta os 60 segundos ate disparar
                try {
                    sleep(6000);
                } catch (InterruptedException e) {
                    p.timeout();
                }
            }).start();
        }
        else{
            while(p == partida) wait();
        }
        return p;
    }
}

class PartidaImpl implements Partida {
    int tentativas = 100;
    int numero = new Random().nextInt(100);
    boolean ganhou = false;
    boolean timeout = false;

    synchronized void timeout() {
        timeout = true;
    }

    public synchronized String adivinha(int n) {
        if (ganhou) return "PERDEU";
        else if (tentativas >= 100) return "TENTATIVAS";
        else if (timeout) return "TEMPO";
        else {
            tentativas++;
            if (n > numero) return "MENOR";
            else if (n < numero) return "MAIOR";
            else {
                ganhou = true;
                return "GANHOU";
            }
        }
    }
}