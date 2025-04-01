package music.fan;
import music.recording.RecordLabel;
import music.recording.Artist;

import static check.CheckThat.*;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.*;
import check.*;

public class FanTest {
    RecordLabel l = new RecordLabel("l", 0);
    RecordLabel l2 = new RecordLabel("l2", 0);
    Artist a1 = new Artist ("a", l);
    Artist a2 = new Artist ("a2", l2);
    Fan f1 = new Fan("f", a1);
    Fan f2 = new Fan("f2", a1);
    Fan f3 = new Fan("f3", a2);
    
    @Test
    public void testName() {
        assertEquals("f", f1.getName());
    }

    @Test
    public void testNoneSpent() {
        assertEquals(0, f1.getMoneySpent());
    }

    @Test
    public void testFaves() {
        assertEquals(true, f1.favesAtSameLabel(f2));
        assertEquals(false, f1.favesAtSameLabel(f3));
    }

    @Test
    public void testMercahndise() {
        assertEquals(10, f1.buyMerchandise(10));
        assertEquals(10, f1.getMoneySpent());
        assertEquals(5, f1.getFavourite().getLabel().getCash());

        assertEquals(9, f1.buyMerchandise(10, f2));
        assertEquals(10+9, f1.getMoneySpent());
        assertEquals(9, f2.getMoneySpent());
        assertEquals(5+4+4, f1.getFavourite().getLabel().getCash());

        assertEquals(8, f1.buyMerchandise(10, f2, f3));
        assertEquals(10+9+8, f1.getMoneySpent());
        assertEquals(9+8, f2.getMoneySpent());
        assertEquals(8, f3.getMoneySpent());
        assertEquals(5+4+4+4+4, f1.getFavourite().getLabel().getCash());
        assertEquals(4, f3.getFavourite().getLabel().getCash());
    }
}
