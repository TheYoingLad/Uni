package music.recording;

import static check.CheckThat.*;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.*;
import check.*;

public class RecordLabelTest {
    RecordLabel l = new RecordLabel("l", 0);
    
    @Test
    public void testName() {
        assertEquals("l", l.getName());
    }

    @Test
    public void testCash() {
        assertEquals(0, l.getCash());
    }

    @Test
    public void testIncome() {
        l.gotIncome(10);
        assertEquals(10, l.getCash());
    }
}
