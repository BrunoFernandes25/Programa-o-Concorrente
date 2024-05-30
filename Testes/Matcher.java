import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

interface Matcher{
    Match waitToPlay(int role) throws InterruptedException;
}

class Match implements Matcher {
    ReentrantLock l = new ReentrantLock();
    Match m = new Match();

    class Player{
        Condition cond = l.newCondition();
        boolean ready = false;
    }

    List<Player> gr = new ArrayList<Player>();
    List<Player> jog = new ArrayList<Player>();


    public Match waitToPlay(int role) throws InterruptedException {
        Match atual;
        Player p = new Player();

        l.lock();
        try{
            atual = m;

            if(role == 0) gr.add(p);
            if(role == 1) jog.add(p);
           
            if(gr.size()>=2 && jog.size()>= 20){
                for(int i = 0;i<2;i++){
                    p = gr.remove(0);
                    p.ready= true;
                    p.cond.signal();
                }
                for(int i = 0;i<20;i++){
                    p = jog.remove(0);
                    p.ready= true;
                    p.cond.signal();
                }

                m = new Match();
            }
            else{
                while(p.ready == false){
                    p.cond.await();
                }
            }
            
            return atual;

        }finally{
            l.unlock();
        }
    }
}