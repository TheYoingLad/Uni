package music.recording;

import static check.CheckThat.*;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.*;
import check.*;

public class ArtistTest {
    RecordLabel l = new RecordLabel("l", 0);
    Artist a = new Artist("a", l);
    
    @Test
    public void testName() {
        assertEquals("a", a.getName());
    }
}
