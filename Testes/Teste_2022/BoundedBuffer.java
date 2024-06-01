import java.util.concurrent.locks.*;

class Boundedbuffer{}

class MatchMaker{
    Lock l = new ReentrantLock();

    public class Pair{
        Condition cond = l.newCondition();
        Boundedbuffer bb = null;
    }

    ArrayList<Pair> produtores = new ArrayList<Pair>();
    ArrayList<Pair> consumidores = new ArrayList<Pair>();

    Boundedbuffer waitForConsumer() throws InterruptedException{
        Pair p;
        l.lock();
        try{
            if(!consumidores.isEmpty()){
                p = consumidores.remove(0);
                p.bb = new Boundedbuffer();
                p.cond.signal();
            }
            else{
                p = new Pair();
                produtores.add(p);
                while(p.bb == null){
                    p.cond.await();
                }
            }
            return p.bb;
        }finally {
            l.unlock();
        }
    }

    Boundedbuffer waitForProducer() throws InterruptedException{
        Pair p;
        l.lock();
        try{
            if(!produtores.isEmpty()){
                p = produtores.remove(0);
                p.bb = new Boundedbuffer();
                p.cond.signal();
            }
            else{
                p = new Pair();
                consumidores.add(p);
                while(p.bb == null){
                    p.cond.await();
                }
            }
            return p.bb;
        }finally {
            l.unlock();
        }
    }

}