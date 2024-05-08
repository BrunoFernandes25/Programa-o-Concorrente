//Exercício 2/3

public class Bank {

    private static class Account {
        private int balance;
        
        Account(int balance) { this.balance = balance; }
        
        int balance() { return balance; }
        
        boolean deposit(int value) {
            balance += value;
            return true;
        }
        
        boolean withdraw(int value) {
            if (value > balance)
                return false;
            balance -= value;
            return true;
        }
    }

    // Bank slots and vector of accounts
    private int slots;
    private Account[] av;

    public Bank(int n) {
        slots=n;
        av=new Account[slots];
        for (int i=0; i<slots; i++) av[i]=new Account(0);
    }

    // Account balance
    public synchronized int balance(int id) {
        if (id < 0 || id >= slots)
            return 0;
        return av[id].balance();
    }
    /*  ou  colocavamos private final int slots;
    * public int balance(int id) {
        if (id < 0 || id >= slots)
            return 0;
        synchronized (this) {return av[id].balance()};       e desta forma temos SC mais pequenas
    }
    * */

    // Deposit
    public synchronized boolean deposit(int id, int value) {
        if (id < 0 || id >= slots)
            return false;
        return av[id].deposit(value);
    }

    // Withdraw; fails if no such account or insufficient balance
    public synchronized boolean withdraw(int id, int value) {
        if (id < 0 || id >= slots)
            return false;
        return av[id].withdraw(value);
    }

    public synchronized boolean transfer(int from, int to, int value){ // com synchronized já não vamos ver transferências a meio
        return withdraw(from,value) && deposit(to,value);
    }

    public synchronized int totalBalance(){ //ao correr isto vemos uma transferência a meio pode ocorrer levantamento da minha conta e ainda nao ter sido depositado no outro cliente
        int total = 0;
        for(int i = 0;i<slots;i++){
            total += balance(i);
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

    Tempo total de execução: 9493 milissegundos
    */