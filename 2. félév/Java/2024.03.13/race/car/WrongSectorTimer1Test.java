package race.car;
import race.car.WrongSectorTimer1;

import static check.CheckThat.*;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.condition.*;
import org.junit.jupiter.api.extension.*;
import org.junit.jupiter.params.*;
import org.junit.jupiter.params.provider.*;
import check.*;

public class WrongSectorTimer1Test{
    WrongSectorTimer1 timer;
    int[] elems = {1,2,3};

    @BeforeEach
    public void initialize(){
        timer = new WrongSectorTimer1(elems);
    }

    @Test
    public void seemsGood(){
        assertEquals(1, timer.sectorTimes[0]);
        assertEquals(2, timer.sectorTimes[1]);
        assertEquals(3, timer.sectorTimes[2]);
    }

    @Test
    public void setArrayElemsBreaksEncapsulation(){
        timer.sectorTimes[0] = -1;
        assertEquals(-1, timer.sectorTimes[0]);
    }
}