package math.operation.textual;

import static check.CheckThat.*;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.condition.*;
import org.junit.jupiter.api.extension.*;
import org.junit.jupiter.params.*;
import org.junit.jupiter.params.provider.*;
import check.*;

public class AdderInvalidTest{
    @Test
    public void wrongInput(){
        assertEquals("Operands are not numbers", new Adder().addAsText("a", "1"));
    }
}