package test;

import fib.Fibonacci;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.*;

class FibonacciTest {

    @ParameterizedTest
    @CsvSource(textBlock = """
             0, -1, Edge case
            -10, -1, Edge case
             1, 1, Base case
             2, 1, Base case
             8, 21, Generic case""")
    void fib_rec(int arg, int expected, String message) {
        assertAll(
                () -> assertEquals(expected, Fibonacci.fib_rec(arg), message + " failed"),
                () -> assertEquals(expected, Fibonacci.fib_iter(arg), message + " failed"),
                () -> assertEquals(Fibonacci.fib_rec(arg), Fibonacci.fib_iter(arg), message + " failed")
        );

    }

    @Test
    void fib_list() {
        assertArrayEquals(new int[] {1, 1, 2, 3, 5, 8, 13, 21, 34, 55}, Fibonacci.fib_array(10));
    }
}