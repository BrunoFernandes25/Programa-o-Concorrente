import processing.core.PApplet;
import processing.core.PImage;

import java.io.IOException;
import java.net.Inet4Address;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Screen extends PApplet {
    PImage sun, ex;
    private final List<PImage> players = new ArrayList<>();
    private final List<PImage> planets = new ArrayList<>();
    private final List<int[]> cores = new ArrayList<>();
    private final Lock stateLock = new ReentrantLock();
    private final StringBuilder usernameInput = new StringBuilder();
    private final StringBuilder passwordInput = new StringBuilder();
    private State state = State.MENU;
    private Server sv;
    private String last_error = "";
    private List<Map.Entry<String, Map.Entry<Integer, Integer>>> top10;

    public static void main(String[] args) {
        PApplet.main("Screen");
    }

    public void settings() {
        String connectionUrl = System.getenv("SERVER_CONNECTION_URL");
        if (connectionUrl == null) {
            connectionUrl = "127.0.0.1:12345";
        }
        String[] ipPort = connectionUrl.split(":");
        try {
            sv = new Server(Inet4Address.getByName(ipPort[0]), Integer.parseInt(ipPort[1]));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        size(1280, 720);
    }

    public void stop() {
        if (sv != null) {
            sv.shutdown();
        }
    }

    public void setup() {
        sun = requestImage("images/sun.png");
        ex = loadImage("images/example.png");
        planets.add(requestImage("images/mercury.png"));
        planets.add(requestImage("images/earth.png"));
        planets.add(requestImage("images/mars.png"));
        planets.add(requestImage("images/neptune.png"));
        players.add(requestImage("images/img1.png"));
        players.add(requestImage("images/img2.png"));
        players.add(requestImage("images/img3.png"));
        players.add(requestImage("images/img4.png"));
        cores.add(new int[]{237, 28, 36});
        cores.add(new int[]{34, 177, 76});
        cores.add(new int[]{255, 242, 0});
        cores.add(new int[]{0, 162, 232});
        imageMode(CENTER);
    }

    public void draw() {
        switch (state) {
            case MENU -> menu();
            case ERROR -> error();
            case LOGIN, REGISTER, DELETE -> inputForm();
            case TOP_UNLOGGED, TOP_LOGGED -> top();
            case LOGGED_IN -> loggedIn();
            case FINDING_MATCH -> lobby();
            case FOUND_MATCH -> found();
            case GAME -> game();
            case WIN_MATCH -> gameWin();
            case LOSE_MATCH -> gameLoss();
            case DRAW_MATCH -> gameDraw();
            case END -> {
            }
        }
    }

    public void keyPressed() {
        State currentState = getState();
        switch (currentState) {
            case REGISTER, LOGIN, DELETE -> {
                if (key == BACKSPACE) {
                    if (focusedField() == 1 && !usernameInput.isEmpty()) {
                        usernameInput.deleteCharAt(usernameInput.length() - 1);
                    } else if (focusedField() == 2 && !passwordInput.isEmpty()) {
                        passwordInput.deleteCharAt(passwordInput.length() - 1);
                    }
                } else if (key == ENTER || key == RETURN) {
                    if (currentState == State.REGISTER) {
                        if (submit(State.REGISTER)) setState(State.MENU);
                        else {
                            last_error = "This user already exists!";
                            setState(State.ERROR);
                        }
                    } else if (currentState == State.LOGIN) {
                        if (submit(State.LOGIN)) setState(State.LOGGED_IN);
                        else {
                            last_error = "Login Fail!";
                            setState(State.ERROR);
                        }
                    } else {
                        if (submit(State.DELETE)) setState(State.MENU);
                        else {
                            last_error = "User does not exist!";
                            setState(State.ERROR);
                        }
                    }
                } else if (key == TAB) setState(State.MENU);
                else {
                    if (focusedField() == 1) {
                        usernameInput.append(key);
                    } else if (focusedField() == 2) {
                        passwordInput.append(key);
                    }
                }
            }
            case WIN_MATCH, DRAW_MATCH, LOSE_MATCH -> {
                if (key == TAB) {
                    setState(State.LOGGED_IN);
                }
            }
            case FINDING_MATCH -> {
                if (key == TAB) {
                    sv.leaveMatchmaking();
                }
            }
            case GAME -> {
                switch (key) {
                    case 'w' -> sv.up();
                    case 'a' -> sv.left();
                    case 'd' -> sv.right();
                }
            }
            case ERROR, TOP_UNLOGGED -> {
                if (key == TAB) setState(State.MENU);
            }
            case TOP_LOGGED -> {
                if (key == TAB) setState(State.LOGGED_IN);
            }
        }
    }

    private void menu() {
        background(33, 22, 42);
        fill(255);
        text("Trabalho Prático - Programação Concorrente", 250, 160);
        image(ex, 900, 450, 600, 330);
        button("Login", 100, height / 2 - 100, State.LOGIN);
        button("Register", 100, height / 2, State.REGISTER);
        button("Delete", 100, height / 2 + 100, State.DELETE);
        button("Top 10", 100, height / 2 + 200, State.TOP_UNLOGGED);
    }

    private void inputForm() {
        background(33, 22, 42);
        fill(200);
        rect((float) width / 2 - 50, (float) height / 2 - 50, 200, 50);
        rect((float) width / 2 - 50, (float) height / 2 + 10, 200, 50);
        fill(255);
        text("Username: ", (float) width / 2 - 261, (float) height / 2 - 20);
        text("Password: ", (float) width / 2 - 250, (float) height / 2 + 40);
        textSize(20);
        text("Press [TAB] to go back to the Menu", (float) width / 2 + 200, height - 50);
        textSize(40);
        text(usernameInput.toString(), (float) width / 2 - 50, (float) height / 2 - 10);
        text(passwordInput.toString(), (float) width / 2 - 50, (float) height / 2 + 50);
    }

    private void top() {
        background(195, 203, 255);
        for (int i = 0; i < top10.size(); i++) {
            var username = top10.get(i).getKey();
            var lvl = top10.get(i).getValue().getKey();
            var wins = top10.get(i).getValue().getValue();
            text((i + 1) + "º lugar: " + username + ", lvl " + lvl + ", " + wins + " wins", 150, 80 + (i * 60));
        }
        textSize(20);
        text("Press [TAB] to go back to the Menu", (float) width / 2 + 200, height - 50);
        textSize(40);
    }

    private void lobby() {
        background(195, 203, 255);
        text("Waiting for a game...", (float) width / 4, (float) height / 2 - 35);
        textSize(20);
        text("Press [TAB] to go back to the Menu", (float) width / 2 + 200, height - 50);
        textSize(40);
    }

    private void found() {
        background(195, 203, 255);
        text("Match found  ...", (float) width / 4, (float) height / 2 - 35);
    }

    private void error() {
        background(33, 22, 42);
        fill(255, 255, 255);
        text(last_error, (float) width / 4, (float) height / 2 - 35);
        textSize(20);
        text("Press [TAB] to go back to the Menu", (float) width / 2 + 200, height - 50);
        textSize(40);
    }

    private void loggedIn() {
        background(195, 203, 255);
        fill(0);
        text("Welcome, " + sv.getUsername() + "!", (float) width / 2 - 590, (float) height / 2 - 50);
        text("Level: " + sv.getLvl(), (float) width / 2 - 550, (float) height / 2 + 20);
        text("Wins: " + sv.getWins(), (float) width / 2 - 550, (float) height / 2 + 70);
        button("Start Playing", width / 2 - 20, height / 2 - 100, State.FINDING_MATCH);
        button("Top 10", width / 2 - 20, height / 2, State.TOP_LOGGED);
        button("Logout", width / 2 - 20, height / 2 + 100, State.MENU);
    }

    private void game() {
        List<GameObject> objects = sv.getGameState();
        int[] cor = cores.get(sv.getId() - 1);
        var fuel = sv.getFuel();
        background(255);
        for (GameObject p : objects) {
            if (p.isPlayer()) {
                pushMatrix();
                translate((float) p.getX(), (float) p.getY());
                rotate((float) p.getAngle());
                image(players.get(p.getId() - 1), 0, 0, 2 * (float) p.getRadius(), 2 * (float) p.getRadius());
                popMatrix();
            } else if (p.isSun()) {
                image(sun, (float) p.getX(), (float) p.getY(), (float) p.getRadius() * 2, (float) p.getRadius() * 2);
            } else {
                image(planets.get(-p.getId() - 1), (float) p.getX(), (float) p.getY(), 2 * (float) p.getRadius(), 2 * (float) p.getRadius());
            }
        }

        if (fuel >= 0.0) {
            fill(200);
            rect(width - 70, 40, 30, 200);
            fill(cor[0], cor[1], cor[2]);
            rect(width - 70, 40 + 200 - (2 * (float) fuel), 30, 2 * (float) fuel);
        }
    }

    private void gameDraw() {
        background(255, 255, 0);
        textSize(100);
        fill(255);
        text("DRAW", 200, 250);
        textSize(20);
        text("Press [TAB] to go back to the Menu", (float) width / 2 + 200, height - 50);
        textSize(40);
    }

    private void gameWin() {
        background(0, 255, 0);
        textSize(100);
        fill(255);
        text("BIG WIN!", 200, 250);
        textSize(20);
        text("Press [TAB] to go back to the Menu", (float) width / 2 + 200, height - 50);
        textSize(40);
    }

    private void gameLoss() {
        background(255, 0, 0);
        textSize(100);
        fill(255);
        text("GAME OVER!", 200, 250);
        textSize(20);
        text("Press [TAB] to go back to the Menu", (float) width / 2 + 200, height - 50);
        textSize(40);
    }

    private boolean submit(State currentState) {
        boolean attempt = switch (currentState) {
            case State.LOGIN -> sv.login(usernameInput.toString(), passwordInput.toString());
            case State.REGISTER -> sv.signup(usernameInput.toString(), passwordInput.toString());
            case State.DELETE -> sv.delete(usernameInput.toString(), passwordInput.toString());
            default -> throw new RuntimeException();
        };
        usernameInput.setLength(0);
        passwordInput.setLength(0);
        return attempt;
    }

    private int focusedField() {
        if (mouseX > width / 2 - 100 && mouseX < width / 2 + 100) {
            if (mouseY > height / 2 - 45 && mouseY < height / 2 - 15) {
                return 1;
            } else if (mouseY > height / 2 + 15 && mouseY < height / 2 + 45) return 2;
        }
        return 0;
    }

    private void button(String text, int x, int y, State buttonState) {
        fill(240, 240, 240);
        rect(x, y, (float) width / 3, (float) height / 8, 15);
        fill(0);
        textSize(40);
        text(text, (float) (x + width / 6 - text.length() * 8), y + (float) height / 13);
        if (mousePressed && mouseX > x && mouseX < x + width / 3 && mouseY > y && mouseY < y + height / 8) {
            setState(buttonState);
            if (text.equals("Logout")) sv.logout();
            if (text.equals("Start Playing")) sv.findMatch(this);
            if (text.equals("Top 10")) top10 = sv.top10();
        }
    }

    public void startGame() {
        setState(State.GAME);
    }

    public void winMatch() {
        setState(State.WIN_MATCH);
    }

    public void loseMatch() {
        setState(State.LOSE_MATCH);
    }

    public void drawMatch() {
        setState(State.DRAW_MATCH);
    }

    private State getState() {
        stateLock.lock();
        var res = state;
        stateLock.unlock();
        return res;
    }

    public void setState(State s) {
        stateLock.lock();
        state = s;
        stateLock.unlock();
    }
}
