package buggy;

public class Buggy {
    public static void timeOut() {
        while (true) {
        }
    }

    public static void zeroDiv() {
        int a = 1 / 0;
    }

    public static void overIndex() {
        int[] a = new int[2];
        int b = a[10];
    }
}
