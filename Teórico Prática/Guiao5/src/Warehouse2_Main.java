import java.util.*;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

//A versão “cooperativa”

class Warehouse2 {
    Lock l = new ReentrantLock();

    private Map<String, Product> map =  new HashMap<String, Product>();

    private class Product {
        //Product(Condition cond){this.cond = cond;} ver linha 25 comentada junto disto era outra forma de fazer
        int quantity = 0;
        Condition cond = l.newCondition();

        /*public void add(int x){
            quantity += x;
            cond.signalAll();
        }

        void remove() throws InterruptedException {
            while (quantity < 1) cond.await();
            quantity--;
        }*/
    }

    private Product get(String item) {
        Product p = map.get(item);
        if (p != null) return p;
        //p = new Product(l.newCondition()); //a cada criacao de produto criamos a sua condiçao relativa ao reentrantLock
        p = new Product();
        map.put(item, p);
        return p;
    }

    //Suply e Consume é que obtem os locks metodos publicos o get de Product assume que já se tem os Locks

    //A operação supply abastece o armazem com uma dada quantidade de um item;
    public void supply(String item, int quantity) {
        l.lock();
        try {
            Product p = get(item);
            p.quantity += quantity;
            p.cond.signalAll(); //acordamos todas aquelas que tem a mesma condição
        }
        finally {
            l.unlock();
        }
    }

    private Product falta_prod(Product[] a) {
        for (Product p : a)
            if (p.quantity == 0) //Se isto acontece alguma nao existe retornando False
                return p;
        return null;
    }

    //A operação consume(Set<String> items) obtém do armazem um conjunto de itens, bloqueando enquanto tal não for possível.
    //Este será o método a alterar de modo a garantira cooperação
    public void consume(Set<String> items) throws InterruptedException{

        //primeiro temos que confirmar que há tudo o que precisamos e tendo exclusão mútua temos de ter atenção aos waits a meio,
        //pois mais a frente posso ja ter libertado algo que preciso
        //Logo, vamos confirmar tudo sem wait, mas se falhar fazer wait do que nos falta

        Product[] a = new Product[items.size()];
        //depois decrementamos todos
        l.lock();
        try{
            int i = 0;
            for (String s : items) {
                a[i++] = get(s);    //array de produtos ja com todos os items
            }
            
            for(;;){
                Product p = falta_prod(a);
                if (p== null) break;;
                p.cond.wait();
            }

            for(Product pp : a)  // p ele referia se a declaracao em cima entao renomeei
                pp.quantity--; //havendo quantidade entao decrementamos
        }
        finally {
            l.unlock();
        }
    }
}

public class Warehouse2_Main {
    public static void main(String[] args) throws InterruptedException {
        // Testando a versão "cooperativa" do armazém
        testCooperativeWarehouse();
    }

    public static void testCooperativeWarehouse() throws InterruptedException {
        System.out.println("Teste da versão cooperativa iniciado.");

        Warehouse2 warehouse = new Warehouse2();

        // Criando threads de fornecedores
        List<Thread> supplierThreads = new ArrayList<>();
        for (int i = 0; i < 20; i++) { // 20 fornecedores
            Thread supplierThread = new Thread(() -> {
                for (int j = 0; j < 10; j++) { // Cada fornecedor faz 10 pedidos de fornecimento
                    warehouse.supply("item2", 5); // Cada pedido adiciona 5 unidades do item2
                }
            });
            supplierThreads.add(supplierThread);
        }

        // Criando threads de consumidores
        List<Thread> consumerThreads = new ArrayList<>();
        for (int i = 0; i < 30; i++) { // 30 consumidores
            Thread consumerThread = new Thread(() -> {
                for (int j = 0; j < 10; j++) { // Cada consumidor faz 10 pedidos de consumo
                    Set<String> items = new HashSet<>();
                    items.add("item2");
                    try {
                        warehouse.consume(items); // Cada pedido tenta consumir o item2
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
            });
            consumerThreads.add(consumerThread);
        }

        // Iniciando todas as threads
        for (Thread thread : supplierThreads) {
            thread.start();
        }
        for (Thread thread : consumerThreads) {
            thread.start();
        }

        // Aguardando todas as threads terminarem
        for (Thread thread : supplierThreads) {
            thread.join();
        }
        for (Thread thread : consumerThreads) {
            thread.join();
        }

        System.out.println("Teste da versão cooperativa concluído.");
    }
}