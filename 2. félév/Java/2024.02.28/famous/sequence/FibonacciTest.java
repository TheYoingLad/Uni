package famous.sequence;

import static check.CheckThat.*;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.condition.*;
import org.junit.jupiter.api.extension.*;
import org.junit.jupiter.params.*;
import org.junit.jupiter.params.provider.*;
import check.*;

public class FibonacciTest{
    @Test
    public void test1() {
        Fibonacci f = new Fibonacci();
        assertEquals(1, f.fib(1));
    }

    @Test
    public void test2() {
        Fibonacci f = new Fibonacci();
        assertEquals(1, f.fib(2));
    }

    @Test
    public void test3() {
        Fibonacci f = new Fibonacci();
        assertEquals(5, f.fib(5));
    }

    @Test
    public void test4() {
        Fibonacci f = new Fibonacci();
        assertEquals(55, f.fib(10));
    }

    @ParameterizedTest(name = "fib({0}) ⟹ {1}")
    @CsvSource(textBlock = """
        1, 1
        3, 2
        4, 3
    """)
    @DisableIfHasBadStructure
    public void testParam(int infib, int outfib) {        
        Fibonacci f = new Fibonacci();
        assertEquals(outfib, f.fib(infib));
    }
}