package famous.sequence;

import static check.CheckThat.*;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.condition.*;
import org.junit.jupiter.api.extension.*;
import org.junit.jupiter.params.*;
import org.junit.jupiter.params.provider.*;
import check.*;

public class TriangularNumbersTest{
    @Test
    public void test1() {
        TriangularNumbers t = new TriangularNumbers();
        assertEquals(0, t.getTriangularNumber(0));
    }

    @Test
    public void test2() {
        TriangularNumbers t = new TriangularNumbers();
        assertEquals(1, t.getTriangularNumber(1));
    }

    @Test
    public void test3() {
        TriangularNumbers t = new TriangularNumbers();
        assertEquals(5050, t.getTriangularNumber(100));
    }

    @Test
    public void test4() {
        TriangularNumbers t = new TriangularNumbers();
        assertEquals(0, t.getTriangularNumber(-1));
    }

    @Test
    public void test5() {
        TriangularNumbers t = new TriangularNumbers();
        assertEquals(0, t.getTriangularNumber(-100));
    }

    @Test
    public void wrong() {
        TriangularNumbers t = new TriangularNumbers();
        assertEquals(100, t.getTriangularNumber(10));
    }

        @Test
    public void testA1() {
        TriangularNumbers t = new TriangularNumbers();
        assertEquals(0, t.getTriangularNumberAlternative(0));
    }

    @Test
    public void testA2() {
        TriangularNumbers t = new TriangularNumbers();
        assertEquals(1, t.getTriangularNumberAlternative(1));
    }

    @Test
    public void testA3() {
        TriangularNumbers t = new TriangularNumbers();
        assertEquals(5050, t.getTriangularNumberAlternative(100));
    }

    @Test
    public void testA4() {
        TriangularNumbers t = new TriangularNumbers();
        assertEquals(0, t.getTriangularNumberAlternative(-1));
    }

    @Test
    public void testA5() {
        TriangularNumbers t = new TriangularNumbers();
        assertEquals(0, t.getTriangularNumberAlternative(-100));
    }

    @Test
    public void wrongA() {
        TriangularNumbers t = new TriangularNumbers();
        assertEquals(100, t.getTriangularNumberAlternative(10));
    }
}