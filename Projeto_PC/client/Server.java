import com.ericsson.otp.erlang.*;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.util.*;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Server {
    private final Socket socket;
    private final GameData data;
    private final OutputStream out;
    private final InputStream in;
    private boolean isLoggedIn = false;
    private String username = "";
    private int id;

    public Server(InetAddress address, int port) throws IOException {                //Esta classe representa a conexão do cliente com o servidor do jogo
        socket = new Socket(address, port);
        in = socket.getInputStream();
        out = socket.getOutputStream();
        data = new GameData(in);                              //Aqui é criado GameData
    }                                                        //Server é responsável por iniciar uma thread de GameData que cuida da recepção e do processamento dos dados do jogo enviados pelo servidor

    public String getUsername() {
        return username;
    }

    public int getLvl() {
        return data.getLvl();
    }

    public int getWins() {
        return data.getWins();
    }

    public int getId() {
        return id;
    }

    public boolean login(String user, String pass) {
        var elements = new OtpErlangObject[]{
                Util.LOGIN,
                new OtpErlangString(user),
                new OtpErlangString(pass),
        };
        Util.send(new OtpErlangTuple(elements), out);
        var response = Util.readTuple(in);
        if (response.elementAt(0).equals(Util.OK)) {
            username = user;
            OtpErlangLong o1 = (OtpErlangLong) response.elementAt(2);
            data.setLvl((int) o1.longValue());
            OtpErlangLong o = (OtpErlangLong) response.elementAt(3);
            data.setWins((int) o.longValue());
            isLoggedIn = true;
        }
        return isLoggedIn;
    }

    public void logout() {
        username = "";
        Util.send(Util.LOGOUT, out);
        isLoggedIn = false;
    }

    public boolean signup(String user, String pass) {
        var elements = new OtpErlangObject[]{
                Util.SIGNUP,
                new OtpErlangString(user),
                new OtpErlangString(pass),
        };
        Util.send(new OtpErlangTuple(elements), out);
        var response = Util.readTuple(in);
        return response.elementAt(0).equals(Util.OK);
    }

    public boolean delete(String user, String pass) {
        var elements = new OtpErlangObject[]{
                Util.DELETE,
                new OtpErlangString(user),
                new OtpErlangString(pass),
        };
        Util.send(new OtpErlangTuple(elements), out);
        var response = Util.readTuple(in);
        return response.elementAt(0).equals(Util.OK);
    }

    public void leaveMatchmaking() {
        Util.send(Util.LEAVE, out);
    }

    public void findMatch(Screen screen) {
        new Thread(() -> {
            Util.send(Util.JOIN, out);
            OtpErlangTuple response = Util.readTuple(in);
            if (response.elementAt(1).equals(Util.START)) {
                id = (int) ((OtpErlangLong) (response.elementAt(2))).longValue();
                data.init(screen);                                                    // Aqui ocorre a inicialização do GameData
            } else {
                screen.setState(State.LOGGED_IN);
            }
        }).start();
    }

    public void left() {
        Util.send(Util.LEFT, out);
    }

    public void right() {
        Util.send(Util.RIGHT, out);
    }

    public void up() {
        Util.send(Util.UP, out);
    }

    public List<Map.Entry<String, Map.Entry<Integer, Integer>>> top10() {
        var elements = new OtpErlangObject[]{
                Util.TOP,
                new OtpErlangInt(10),
        };
        Util.send(new OtpErlangTuple(elements), out);
        var response = Util.readTuple(in);
        var ListN = (OtpErlangList) response.elementAt(2);
        var res = new ArrayList<Map.Entry<String, Map.Entry<Integer, Integer>>>();
        ListN.forEach(i -> {
            var tup = (OtpErlangTuple) i;
            var username = ((OtpErlangString) tup.elementAt(0)).stringValue();
            OtpErlangLong o2 = (OtpErlangLong) tup.elementAt(1);
            var lvl = (int) o2.longValue();
            OtpErlangLong o1 = (OtpErlangLong) tup.elementAt(2);
            var wins = (int) o1.longValue();
            res.add(Map.entry(username, Map.entry(lvl, wins)));
        });
        return res;
    }

    public List<GameObject> getGameState() {
        return data.getData();
    }

    public double getFuel() {
        return data.getFuel();
    }

    public void shutdown() {
        data.interrupt();
        try {
            socket.close();
        } catch (IOException e) {

        }
    }
}

class GameData extends Thread {
    private final InputStream input;
    private final Lock dataLock = new ReentrantLock();
    private final Lock fuelLock = new ReentrantLock();
    private final Lock lvlLock = new ReentrantLock();
    private final Lock winsLock = new ReentrantLock();
    private Screen screen;
    private List<GameObject> data = new ArrayList<>();
    private int lvl = 1;
    private int wins = 0;
    private double fuel;
    private boolean ready = false;

    public GameData(InputStream in) {
        input = in;
        start();
    }

    public int getLvl() {                //Contém informações como nível do jogador
        lvlLock.lock();
        var res = lvl;
        lvlLock.unlock();
        return res;
    }

    public void setLvl(int newLvl) {        
        lvlLock.lock();
        lvl = newLvl;
        lvlLock.unlock();
    }

    public int getWins() {                //número de vitórias 
        winsLock.lock();
        var res = wins;
        winsLock.unlock();
        return res;
    }

    public void setWins(int newWins) {
        winsLock.lock();
        wins = newWins;
        winsLock.unlock();
    }

    public double getFuel() {    //quantidade de combustível disponível
        fuelLock.lock();
        var res = fuel;
        fuelLock.unlock();
        return res;
    }

    private void setFuel(double newFuel) {
        fuelLock.lock();
        fuel = newFuel;
        fuelLock.unlock();
    }

    public List<GameObject> getData() {
        dataLock.lock();
        var res = data;
        dataLock.unlock();
        return res;
    }

    private void setData(List<GameObject> newData) {
        dataLock.lock();
        data = newData;
        dataLock.unlock();
    }

    public synchronized void init(Screen newScreen) {
        if (ready) return;
        screen = newScreen;
        ready = true;
        fuel = 100.0;
        notify();
        screen.startGame();
    }

    @Override
    public void run() {
        while (!Thread.currentThread().isInterrupted()) {
            try {
                synchronized (this) {
                    ready = false;
                    while (!ready) wait();
                }
            } catch (InterruptedException e) {
                break;
            }
            while (!Thread.currentThread().isInterrupted()) {
                var tup = Util.readTuple(input);
                if (tup.elementAt(1).equals(Util.WIN)) {
                    OtpErlangLong o1 = (OtpErlangLong) tup.elementAt(2);
                    setLvl((int) o1.longValue());
                    OtpErlangLong o = (OtpErlangLong) tup.elementAt(3);
                    setWins((int) o.longValue());
                    screen.winMatch();
                    break;
                } else if (tup.elementAt(1).equals(Util.LOSS)) {
                    OtpErlangLong o1 = (OtpErlangLong) tup.elementAt(2);
                    setLvl((int) o1.longValue());
                    OtpErlangLong o = (OtpErlangLong) tup.elementAt(3);
                    setWins((int) o.longValue());
                    screen.loseMatch();
                    break;
                } else if (tup.elementAt(1).equals(Util.DRAW)) {
                    OtpErlangLong o1 = (OtpErlangLong) tup.elementAt(2);
                    setLvl((int) o1.longValue());
                    OtpErlangLong o = (OtpErlangLong) tup.elementAt(3);
                    setWins((int) o.longValue());
                    screen.drawMatch();
                    break;
                } else if (tup.elementAt(1).equals(Util.FUEL)) {
                    OtpErlangDouble o = (OtpErlangDouble) tup.elementAt(2);
                    setFuel(o.doubleValue());
                } else if (tup.elementAt(1).equals(Util.UPDATE)) {
                    var list = (OtpErlangList) tup.elementAt(2);
                    setData(Arrays.stream(list.elements())
                            .filter(Objects::nonNull)
                            .map(GameObject::new)
                            .toList());
                }
            }
        }
    }
}
