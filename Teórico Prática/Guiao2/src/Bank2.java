//AQUI O QUE MUDOU? COLOCAMOS syncronized NOS MÉTODOS DAS CONTAS EM VEZ DO BANCO 

public class Bank2 {

    private static class Account {
        private int balance;

        Account(int balance) {
            this.balance = balance;
        }

        synchronized int balance() {
            return balance;
        }

        synchronized boolean deposit(int value) {
            balance += value;
            return true;
        }

        synchronized boolean withdraw(int value) {
            if (value > balance)
                return false;
            balance -= value;
            return true;
        }
    }

    // Bank slots and vector of accounts
    private int slots;
    private Account[] av;

    public Bank2(int n) {
        slots = n;
        av = new Account[slots];
        for (int i = 0; i < slots; i++) av[i] = new Account(0);
    }

    // Account balance
    public int balance(int id) {
        if (id < 0 || id >= slots)
            return 0;
        return av[id].balance();
    }

    // Deposit
    public boolean deposit(int id, int value) {
        if (id < 0 || id >= slots)
            return false;
        return av[id].deposit(value);
    }

    // Withdraw; fails if no such account or insufficient balance
    public boolean withdraw(int id, int value) {
        if (id < 0 || id >= slots)
            return false;
        return av[id].withdraw(value);
    }

    public boolean transfer(int from, int to, int value) {
        if (from < 0 || from >= slots || to < 0 || to >= slots) return false;
        Account cfrom = av[from];
        Account cto = av[to];

        Account l1, l2;     //Evitar deadlocks vendo qual o menor e dar lock ao menor

        if (from < to) {
            l1 = cfrom;
            l2 = cto;
        } else {
            l1 = cto;
            l2 = cfrom;
        }

        synchronized (l1) {     //depois fazer synchronized sobre o objeto com menor numero de conta e depois o seguinte
            synchronized (l2) {
                if (!cfrom.withdraw(value)) return false;   //falhando a condição significa que o levantamento foi feito
                return cto.deposit(value);      //falta então depositar na conta respetiva
            }
        }
    }

    //Mas nos nao queremos recursividade, queremos algo melhor
    /*public int totalBalance() {
        return totalBalanceRec(0);
    }

    public int totalBalanceRec(int ind) {
        if (ind < slots) {
            synchronized (av[ind]) {
                return totalBalanceRec(ind + 1);
            }
        } else {
            int total = 0;
            for (int i = 0; i < slots; i++)
                total += balance(i);
            return total;
        }
    }*/

    public int totalBalance() {
        int total = 0;
        synchronized (this) { // Bloqueia o banco para garantir consistência dos dados
            for (int i = 0; i < slots; i++) {
                total += av[i].balance(); // Obtém o saldo de cada conta e soma ao total
            }
        }
        return total;
    }
}

/*
Para: 
    int ACCS = 10;
    int ITERS = 1000000000;
    
    obtemos um bloqueio
 
Para:
    int ACCS = 10;
    int ITERS = 100000000;

    Tempo total de execução: 13032 milissegundos   [Este tempo deve-se ao totalBalance ser recursivo]

    Com totalBalance() atual temos:
    Tempo total de execução: 11411 milissegundos
    */