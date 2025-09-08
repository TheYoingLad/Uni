package alma.fa.bigyusz.cucc;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvFileSource;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.*;

class CardTest {
    @ParameterizedTest
    @CsvFileSource(files = "alma.csv")
    void test1(int expected, String values) {
        var bigyusz = Arrays.stream(values.split(";"))
                .map(Card::new)
                .mapToInt(card -> card.ertek().numeric())
                .sum();
        assertEquals(expected, bigyusz);
    }

    @Test
    void test2() {
    }
}