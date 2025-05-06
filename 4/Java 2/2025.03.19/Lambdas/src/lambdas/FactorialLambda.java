package lambdas;

import java.util.function.IntUnaryOperator;

public class FactorialLambda {
    public static IntUnaryOperator factIter = n -> {
        int fact = 1;
        for (int i = 2; i <= n; i++) fact *= i;
        return fact;
    };

    public static IntUnaryOperator factRec = new IntUnaryOperator() {
        @Override
        public int applyAsInt(int n) {
            if(n == 1) return 1;
            return n*this.applyAsInt(n-1);
        }
    };

    public static void main(String[] args) {
        System.out.println(factIter.applyAsInt(5));
        System.out.println(factRec.applyAsInt(5));
    }
}
