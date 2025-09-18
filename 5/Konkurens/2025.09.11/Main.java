import java.io.FileNotFoundException;
import java.io.PrintWriter;

public class Main {
    public static final int iterations = 1000;

    public static void main(String[] args) throws InterruptedException {
        ThreadGroup tg = new ThreadGroup("Thread Group");

        Thread t1 = new MyThread(tg);

        Thread t2 = new Thread(tg, new MyRunnable());

        Thread t3 = new Thread() {
            @Override
            public void run() {

                for (int i = 0; i < iterations; i++)
                    System.out.println("Anoním Thread");
            }
        };


        Thread t4 = new Thread(tg, new Runnable(){
            @Override
            public void run() {
                for (int i = 0; i < iterations; i++)
                    System.out.println("Anoním Runnable");
            }
        });

        Thread t5 = new Thread(() -> {
            for (int i = 0; i < iterations; i++)
                System.out.println("Thread konstrukor");
        });

        t1.setName("Thread 1");
        t2.setName("Thread 2");
        t3.setName("Thread 3");
        t4.setName("Thread 4");
        t5.setName("Thread 5");


        t1.start();
        t2.start();
        t3.start();
        t4.start();
        t5.start();

        int a = tg.activeCount();
    }
}

class MyThread extends Thread {
    public MyThread(ThreadGroup group) {
        super(group, "");
    }

    @Override
    public void run() {
        for (int i = 0; i < Main.iterations; i++)
            System.out.println("MyThread");
    }
}

class MyRunnable implements Runnable {
    @Override
    public void run() {
        for (int i = 0; i < Main.iterations; i++)
            System.out.println("MyRunnable");
    }
}