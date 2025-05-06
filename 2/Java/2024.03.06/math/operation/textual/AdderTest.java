package math.operation.textual;

import static check.CheckThat.*;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.condition.*;
import org.junit.jupiter.api.extension.*;
import org.junit.jupiter.params.*;
import org.junit.jupiter.params.provider.*;
import check.*;

public class AdderTest{
    @Test
    public void addZero(){
        assertEquals("0", new Adder().addAsText("0", "0"));
        assertEquals("1", new Adder().addAsText("1", "0"));
        assertEquals("1", new Adder().addAsText("0", "1"));
        assertEquals("1.0", new Adder().addAsText("0.0", "1"));
        assertEquals("1.0", new Adder().addAsText("0", "1.0"));
        assertEquals("1.0", new Adder().addAsText("0.0", "1.0"));
    }

    @Test
    public void add(){
        assertEquals("3", new Adder().addAsText("1", "2"));
        assertEquals("3", new Adder().addAsText("2", "1"));
        assertEquals("3.0", new Adder().addAsText("2.0", "1"));
        assertEquals("3.0", new Adder().addAsText("2", "1.0"));
        assertEquals("3.0", new Adder().addAsText("2.0", "1.0"));
    }

    @Test
    public void addCommutativa(){
        assertEquals(new Adder().addAsText("2", "1"), new Adder().addAsText("1", "2"));
        assertEquals(new Adder().addAsText("2.0", "1"), new Adder().addAsText("1", "2.0"));
        assertEquals(new Adder().addAsText("2", "1.0"), new Adder().addAsText("1.0", "2"));
        assertEquals(new Adder().addAsText("2.0", "1.0"), new Adder().addAsText("1.0", "2.0"));
    }
}