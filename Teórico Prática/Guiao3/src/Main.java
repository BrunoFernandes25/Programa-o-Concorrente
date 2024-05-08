import java.util.*;
import java.util.concurrent.locks.*;

class NotEnoughFunds extends Exception {}
class InvalidAccount extends Exception {}

//Guião 3 mudou a criação e fecho de conta e o totalBalance em que podemos saber o saldo de x contas que passemos por parâmetros

class Bank {

    private static class Account {
        private int balance;
        private Lock l = new ReentrantLock(); //l -> Lock das contas

        Account(int balance) {
            this.balance = balance;
        }

        int balance() {
            return balance;
        }

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

    private Map<Integer, Account> map = new HashMap<Integer, Account>();    // {nºconta,conta}
    private int nextId = 0;
    private Lock lBank = new ReentrantLock(); //lBank -> lock do banco
    //Com leitura e escrita fariamos:
    // private ReentrantReadWriteLock rwl = new ReentrantReadWriteLock(); e fariamos para criar conta, como mexe na hashmap
    //apenas o Balance e TotalBalance são de leitura tudo o resto é escrita

    // create account and return account id
    public int createAccount(int balance) {
        Account c = new Account(balance); //torna mais eficiente o nosso codigo, pois assim permite que nao seja apenas eu a usar o Banco
        lBank.lock(); //adquirir lock Banco
        //rwl.writeLock().lock();
        //conta é criada mas ninguem a conhece logo nao precisamos de dar lock já nesta conta daremos sim quando usarmos metodos sobre essa conta
        try {
            //Account c = new Account(balance);
            int id = nextId;
            nextId += 1;
            map.put(id, c);
            return id;
        } finally {
            lBank.unlock();
            //rwl.writeLock().unlock();
        }
    }

    // close account and return balance, or 0 if no such account
    public int closeAccount(int id) {
        Account c;
        lBank.lock();
        //rwl.writeLock().lock();
        try {
            c = map.remove(id);
            //c.lBank.lock() é má ideia c pode ser NULL
            if (c == null)
                return 0;
            //existindo conta damos lock à mesma
            c.l.lock();
        }finally {
            lBank.unlock();  //como nao existe conta damos unlock ao Banco
            //rwl.writeLock().unlock();
        }
        try{
            return c.balance();
        }finally {
            c.l.unlock();
        }
    }

    //Com synchronized nao conseguiriamos fazer isto pois nao temos alinhamento

    /*  desta forma só uma Thread ia poder usar o Banco e não é isso que queremos
    *     lBank.lock();
        try{
            Account c = map.remove(id);
            if (c == null)
                return 0;
            return c.balance();
        }finally {
            lBank.unlock();
        }
    }
    *
    * e podemos ter Trheads a querer mexer na conta a remover podem ainda nao saber que a removemos
    * entao convem dar lock na conta e depois unlock
    * */

    // account balance; 0 if no such account
    public int balance(int id) {
        Account c;
        lBank.lock();
        //rwl.readLock().lock();
        try{
            c = map.get(id);
            if (c == null)
                return 0;
            c.l.lock();
        }finally {
            lBank.unlock();
            //rwl.readLock().unlock();
        }
        try{
            return c.balance();
        }finally {
            c.l.unlock();
        }
    }

    // deposit; fails if no such account
    public boolean deposit(int id, int value) {
        Account c;
        lBank.lock();
        try{
            c = map.get(id);
            if (c == null)
                return false;
            c.l.lock();
        }finally {
            lBank.unlock();
        }
        try{
            return c.deposit(value);
        }finally {
            c.l.unlock();
        }
    }

    // withdraw; fails if no such account or insufficient balance
    public boolean withdraw(int id, int value) {
        Account c;
        try {
            c = map.get(id);
            if (c == null)
                return false;
            c.l.lock();
        }finally {
            lBank.unlock();
        }
        try{
            return c.withdraw(value);
        }finally {
            c.l.unlock();
        }
    }

    // transfer value between accounts;
    // fails if either account does not exist or insufficient balance
    public boolean transfer(int from, int to, int value) {
        Account cfrom, cto,l1,l2;
        lBank.lock();
        try {
            cfrom = map.get(from);
            cto = map.get(to);
            if (cfrom == null || cto == null)
                return false;

            if (from < to) {
                l1 = cfrom;
                l2 = cto;
            } else {
                l1 = cto;
                l2 = cfrom;
            }

            l1.l.lock();
            l2.l.lock();
        }finally {
            lBank.unlock();
        }

        try{
            return cfrom.withdraw(value) && cto.deposit(value); //poderiamos ainda otimizar ao fim de levantar o dinheiro de cfrom dar unlock a conta respetiva
        }finally {
            l1.l.unlock();
            l2.l.unlock();
        }
    }

    // sum of balances in set of accounts; 0 if some does not exist
    public int totalBalance(int[] ids) {    //queremos dar lock a todas as contas antes de dar unlock ao banco
        ids = ids.clone();
        Arrays.sort(ids); //sem esta ordem poderia ter deadlock (Lembrar que devemos ter sempre os locks pelo menor id)
        Account[] acs = new Account[ids.length]; //ids.length é o numero de contas que existem
        lBank.lock();
        try{
            for (int i = 0;i<ids.length;i++) {
                acs[i] = map.get(ids[i]);
                if (acs[i] == null)
                    return 0;
                //acs[i].l.lock(); isto ia dar problemas pois podemos nao chegar ao fim ou nao existir a conta(acs[i] = null) e perdemos tudo o que adquirimos
            }
            for(int i = 0;i<ids.length;i++){
                acs[i].l.lock();    //assim ja obtivemos todas as contas e podemos dar lock nelas
            }
        }finally {
            lBank.unlock();
        }

        int total = 0;
        for (int i = 0; i <ids.length;i++){
            try{
                total += acs[i].balance();
            }
            finally {
                acs[i].l.unlock();
            }
        }
        return total;
    }

    int accountBalance(int id) throws InvalidAccount {
        Account a;
        lBank.lock();
        try {
            a = map.get(id);
            if (a == null)
                throw new InvalidAccount();
            a.l.lock();
        } finally {
            lBank.unlock();
        }
    
        try {
            return a.balance();
        } finally {
            a.l.unlock();
        }
    }

}

public class Main {
    public static void main(String args[]) {
            try {
                int accounts[] = new int[10];
                int n = 10;
                Bank b = new Bank();

                for (int i = 0; i < n; i++) accounts[i] = b.createAccount(i+1);
                for (int i = 0; i < n; i++) b.deposit(accounts[i], 100*(i+1));

                printAccountBalances(accounts, b, n);

                System.out.println("Closed account 5 with $" + b.closeAccount(5) + ".");

                int newList[] = {0,1,2,3,4,6,7,8,9};
                System.out.println("Total Balance: $" + b.totalBalance(newList) + ".\n");
                b.transfer(9, 0, 300);
                System.out.println("Account[9]:Transfer $300 to Account [0].\n");
                printAccountBalances(accounts, b, n);
                System.out.println("Total Balance: $" + b.totalBalance(newList) + ".\n");
            } catch (Exception e) {
                e.printStackTrace();
            }

    }

    public static void printAccountBalances(int accounts[], Bank b, int n) throws InvalidAccount, NotEnoughFunds {
        for (int i = 0; i < n; i++) {
            try {
                System.out.println("Account " + i + ": " + b.accountBalance(accounts[i]));
            } catch (InvalidAccount ia) {
                System.out.println("Account "+ i + " does not exist.");
            }
        }
    }
}



//Teremos que ter cuidado no CC(controlo de concorrência) pois temos um Banco mutavel(HashMap)
// e podem occorrer escritas e leituras a quererem ser feitas ao mesmo tempo.


//Adquirir lock do banco se mexer no nextId ou na HashMap
//Se for para mexer numa conta no balance, devemos ter adquirido o lock da conta respetiva


//Se uma Thread tem lock do Banco e lock de uma conta nenhuma outra thread tem lock de contas pois nao tem lock do Banco

//os if the elses com leitura e escrita que fizemos para obter a ordem nao é precisa acho