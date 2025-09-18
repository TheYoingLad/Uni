public class Main {
    public static void main(String[] args) throws InterruptedException {
        Thread t1 = new Thread(() -> {
            for (int i = 0; i < 10_000; i++){
                System.out.print("h");
                System.out.print("e");
                System.out.print("l");
                System.out.print("o");
            }
        });

        Thread t2 = new Thread(() -> {
            for (int i = 0; i < 10_000; i++) {
                System.out.print("a");
                System.out.print("b");
                System.out.print("c");
            }
        });

        t1.start();
        t2.start();

        t1.join();
        t2.join();
    }
}