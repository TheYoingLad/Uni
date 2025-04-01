package lambdas;

import java.util.Random;
import java.util.function.IntConsumer;

public class PrinterLambdas {
    public static IntConsumer nTimes = (n) -> {
        for (int i = 0; i < n; i++) System.out.println(n);
    };

    public static IntConsumer randomTimes = (n) -> {
        int times = new Random().nextInt(0,11);
        for (int i = 0; i < times; i++) System.out.println(n);
    };

    private static int[] called = {1};
    public static IntConsumer calledTimes = (n) -> {
        for (int i = 0; i < called[0]; i++) System.out.println(n);
        called[0]++;
    };

    public static void main(String[] args) {
        nTimes.accept(3);
        System.out.println();

        randomTimes.accept(10);
        randomTimes.accept(2);
        System.out.println();

        calledTimes.accept(1);
        calledTimes.accept(2);
        calledTimes.accept(3);
    }
}
