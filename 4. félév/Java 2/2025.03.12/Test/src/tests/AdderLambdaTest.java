package tests;

import static org.junit.jupiter.api.Assertions.*;

import lambdas.AdderLambdas;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.util.function.IntSupplier;

public class AdderLambdaTest {
    @ParameterizedTest
    @CsvSource(textBlock = """
            1
            10
            3
            """)
    public void outsideTest(int count) {
        var countUp = AdderLambdas.intsFactory.get();
        Executable[] execs = new Executable[count];
            for (int i = 0; i < count; i++) {
                int i2 = i+1;
                execs[i] = () -> assertEquals(i2, countUp.getAsInt());
        }
        assertAll(execs);
    }

    @Test
    void insideTest() {
        int[] result = {
                AdderLambdas.intsInside.getAsInt(),
                AdderLambdas.intsInside.getAsInt(),
                AdderLambdas.intsInside.getAsInt(),
                AdderLambdas.intsInside.getAsInt(),
                AdderLambdas.intsInside.getAsInt()
        };
        assertAll(
                () -> assertEquals(1, result[0]),
                () -> assertEquals(2, result[1]),
                () -> assertEquals(3, result[2]),
                () -> assertEquals(4, result[3]),
                () -> assertEquals(5, result[4])
        );
    }
//    @Test
//    void insideTest2() {
//        assertEquals(1, AdderLambda.intsInside.getAsInt());
//    }

    @ParameterizedTest
    @CsvSource(textBlock = """
            1
            10
            3
            """)
    public void factoryTest(int count) {
        var countUp = AdderLambdas.intsFactory.get();
        Executable[] execs = new Executable[count];
        for (int i = 0; i < count; i++) {
            int i2 = i+1;
            execs[i] = () -> assertEquals(i2, countUp.getAsInt());
        }
        assertAll(execs);
    }
}
