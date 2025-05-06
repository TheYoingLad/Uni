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

public class SingleLineFileTest{
    @Test
    public void exceptionTest() throws IOException{
        try{
            SingleLineFile.addNumbers("text/to/numbers/empty.txt");
            fail();
        } catch(IllegalArgumentException e){
            System.out.println(e.getMessage());
        }
    }

    @Test
    public void addTest() throws IOException{
        assertEquals(-117, SingleLineFile.addNumbers("text/to/numbers/input.txt"));
    }
}