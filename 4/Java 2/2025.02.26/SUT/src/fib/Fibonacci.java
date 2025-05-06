package fib;

import java.util.List;

public class Fibonacci {
    public static int fib_rec(int n) {
        if (n <= 0) return -1;
        if (n == 1 || n == 2) return 1;
        return fib_rec(n - 1) + fib_rec(n - 2);
    }

    public static int fib_iter(int n) {
        if (n <= 0) return -1;
        int prev2 = 1;
        int prev1 = 1;
        for (int i = 0; i < n - 2; i++) {
            int current = prev1 + prev2;
            prev2 = prev1;
            prev1 = current;
        }
        return prev1;
    }

    public static int[] fib_array(int n){
        int[] fibs = new int[n];
        fibs[0] = 1;
        fibs[1] = 1;
        for(int i = 2; i < n; i++){
            fibs[i] = fibs[i-1] + fibs[i-2];
        }
        return fibs;
    }
}