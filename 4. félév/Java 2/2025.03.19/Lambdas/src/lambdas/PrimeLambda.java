package lambdas;

import java.util.ArrayList;
import java.util.List;
import java.util.function.IntFunction;
import java.util.function.IntPredicate;

public class PrimeLambda {
    public static IntPredicate isPrime = n -> {
        for (int i = 2; i <= Math.sqrt(n); i++) {
            if (n % i == 0) return false;
        }
        return true;
    };

    public static IntFunction<List<Integer>> getPrimesToN = n ->{
        List<Integer> primes = new ArrayList<>();
        for (int i = 2; i <= n; i++) if(isPrime.test(i)) primes.add(i);
        return primes;
    };

    public static void main(String[] args) {
        System.out.println("2: " + isPrime.test(2));
        System.out.println("3: " + isPrime.test(3));
        System.out.println("10: " + isPrime.test(10));
        System.out.println("19: " + isPrime.test(19));
        System.out.println();

        System.out.println(getPrimesToN.apply(50));
    }
}
