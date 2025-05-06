package lambdas;

import java.util.function.IntUnaryOperator;

public class FibonacciLambda {
    public static IntUnaryOperator fibIter = n -> {
        int prev = 1;
        int current = 1;
        for (int i = 1; i < n; i++) {
            int helper = current;
            current += prev;
            prev = helper;
        }
        return current;
    };

    public static IntUnaryOperator fibRec = new IntUnaryOperator() {
        @Override
        public int applyAsInt(int n) {
            if(n <= 1) return 1;
            return this.applyAsInt(n-1) + this.applyAsInt(n-2);
        }
    };

    public static void main(String[] args) {
        System.out.println(fibIter.applyAsInt(5));
        System.out.println(fibRec.applyAsInt(5));
    }
}
