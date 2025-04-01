package math.operation.safe;

import static check.CheckThat.*;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.condition.*;
import org.junit.jupiter.api.extension.*;
import org.junit.jupiter.params.*;
import org.junit.jupiter.params.provider.*;
import check.*;

public class IncrementTest {
    @Test
    public void test1() {
        assertEquals(1, new Increment().increment(0));
    }

    @Test
    public void test2() {
        assertEquals(Integer.MIN_VALUE + 1, new Increment().increment(Integer.MIN_VALUE));
    }

    @Test
    public void test3() {
        assertEquals(Integer.MAX_VALUE, new Increment().increment(Integer.MAX_VALUE));
    }

    @Test
    public void test4() {
        assertEquals(10000, new Increment().increment(9999));
    }

    @Test
    public void test5() {
        assertEquals(-10000, new Increment().increment(-10001));
    }

    @Test
    public void test6() {
        assertEquals(0, new Increment().increment(-1));
    }
}