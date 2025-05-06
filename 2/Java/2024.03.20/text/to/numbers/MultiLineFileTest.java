package text.to.numbers;

import static check.CheckThat.*;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.condition.*;
import org.junit.jupiter.api.extension.*;
import org.junit.jupiter.params.*;
import org.junit.jupiter.params.provider.*;
import check.*;

import java.io.IOException;

public class MultiLineFileTest{
    @Test
    public void addTestEmpty() throws IOException{
        assertEquals(0, MultiLineFile.addNumbers("text/to/numbers/empty.txt", ' '));
    }

    @Test
    public void addTest1() throws IOException{
        assertEquals(-117, MultiLineFile.addNumbers("text/to/numbers/input.txt", ' '));
    }

    @Test
    public void addTest2() throws IOException{
        assertEquals(-117, MultiLineFile.addNumbers("text/to/numbers/input2.txt", ' '));
    }

    @Test
    public void addTestColon() throws IOException{
        assertEquals(0, MultiLineFile.addNumbers("text/to/numbers/input.txt", ','));
    }
}