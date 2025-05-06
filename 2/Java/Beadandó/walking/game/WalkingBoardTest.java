package walking.game;

import walking.game.util.*;

import static check.CheckThat.*;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.condition.*;
import org.junit.jupiter.api.extension.*;
import org.junit.jupiter.params.*;
import org.junit.jupiter.params.provider.*;
import check.*;
import java.util.stream.*;

public class WalkingBoardTest {
    @ParameterizedTest(name = "testing wiht size {0}")
    @ValueSource(ints = {1,2,3})
    public void testSimpleInit(int size){
        WalkingBoard w = new WalkingBoard(size);
        assertEquals(3, w.getTile(0, 0));
        assertEquals(3, w.getTile(size-1, 0));
        assertEquals(3, w.getTile(0, size-1));
        assertEquals(3, w.getTile(size-1, size-1));
        int[][] ww = w.getTiles();
        assertEquals(size, ww.length);
        assertEquals(size, ww[0].length);
    }

    @ParameterizedTest(name = "value should be {2} at ({0},{1})")
    @CsvSource(textBlock = """
        1, 0, 5
        0, 0, 3
    """)
    public void testCustomInit(int x, int y, int expected){
        int[][] source = {
            {1,2,3,4},
            {5},
            {3,4}
        };
        WalkingBoard w = new WalkingBoard(source);
        for(int i = 0; i < source.length; i++) for(int j = 0; j < source[i].length; j++) assertEquals(Math.max(source[i][j], w.BASE_TILE_SCORE), w.getTile(i, j));

        source[x][y] = expected + 1;
        assertEquals(expected, w.getTile(x, y));

        int[][] ww = w.getTiles();
        ww[x][y] = expected + 1;
        assertEquals(expected, w.getTile(x, y));
    }
    
    @Test
    public void testMoves() {
        WalkingBoard w = new WalkingBoard(3);
        assertEquals(0, w.moveAndSet(Direction.UP, 2));
        assertEquals(3, w.moveAndSet(Direction.DOWN, 2));
        assertEquals(3, w.moveAndSet(Direction.DOWN, 2));
        assertEquals(2, w.moveAndSet(Direction.UP, 1));
        assertEquals(0, w.moveAndSet(Direction.LEFT, 2));
    }
}