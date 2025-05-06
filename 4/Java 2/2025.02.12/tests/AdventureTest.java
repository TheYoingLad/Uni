package tests;

import exam.sinbad.Adventure;
import exam.sinbad.sky.Ankaa;
import exam.sinbad.sky.HeightRange;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class AdventureTest {

    @Test
    void getCollectedDiamonds() {
        Adventure a = new Adventure(new Ankaa("B", 1));
        assertEquals(0, a.getCollectedDiamonds());

        Adventure b = new Adventure();
        assertEquals(0, b.getCollectedDiamonds());
    }

    @Test
    void getBirdCount() {
        Adventure a = new Adventure(new Ankaa("A", 1), new Ankaa("B", 2));
        assertEquals(2, a.getBirdCount());

        Adventure b = new Adventure();
        assertEquals(1, b.getBirdCount());
    }

    @Test
    void getHeightRange() {
        assertEquals(HeightRange.LOW, HeightRange.getHeightRange(0));
        assertEquals(HeightRange.LOW, HeightRange.getHeightRange(299));
        assertEquals(HeightRange.LOW, HeightRange.getHeightRange(300));
        assertEquals(HeightRange.MEDIUM, HeightRange.getHeightRange(301));
        assertEquals(HeightRange.MEDIUM, HeightRange.getHeightRange(600));
        assertEquals(HeightRange.HIGH, HeightRange.getHeightRange(601));
        assertEquals(HeightRange.HIGH, HeightRange.getHeightRange(900));
        assertEquals(HeightRange.BEYOND, HeightRange.getHeightRange(901));
    }

    @Test
    void testToString() {
        assertEquals("alma[flying LOW at 76 meters]", new Ankaa("alma", 76).toString());
        assertEquals("bela[flying MEDIUM at 400 meters]", new Ankaa("bela", 400).toString());
        assertEquals("cecil[flying HIGH at 765 meters]", new Ankaa("cecil", 765).toString());
        assertEquals("OneDoesNotSimplyFlyOutOfDiamondIsland[flying BEYOND at 7650 meters]", new Ankaa("magas", 7650).toString());
    }

    @Test
    void testEquals() {
        Ankaa a1 = new Ankaa("egy", 2);
        Ankaa a2 = new Ankaa("egy", 2);
        Ankaa a3 = new Ankaa("egy", 20);
        Ankaa a4 = new Ankaa("ketto", 20);

        assertEquals(a1, a2);
        assertEquals(a1, a3);

        assertNotEquals(a3, a4);
    }

    @Test
    void callBird() {
        Adventure a = new Adventure();
        assertFalse(a.callBird(100));
        assertFalse(a.callBird(1000));
        assertEquals(1, a.getDay());

        a = new Adventure();
        a.collectDiamonds(10);
        assertTrue(a.callBird(650));
        assertEquals(0, a.getCollectedDiamonds());
        assertEquals(10, Adventure.getStoredDiamonds());
        assertEquals(2, a.getDay());

        a = new Adventure();
        a.clearStoredDiamonds();
        a.collectDiamonds(1);
        assertTrue(a.callBird(601));
        a.collectDiamonds(2);
        assertTrue(a.callBird(650));
        a.collectDiamonds(3);
        assertTrue(a.callBird(800));
        assertEquals(0, a.getCollectedDiamonds());
        assertEquals(6, Adventure.getStoredDiamonds());
        assertEquals(4, a.getDay());
    }
}