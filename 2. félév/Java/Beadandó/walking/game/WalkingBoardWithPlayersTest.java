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

public class WalkingBoardWithPlayersTest {
    @Test
    public void walk1() {
        WalkingBoardWithPlayers w = new WalkingBoardWithPlayers(3,2);
        int[][] expectedB = {
            {3,1,2},
            {5,4,3},
            {3,3,3}
        };
        int[] expectedS = {6,9};
        int[] s = w.walk(1,1,1,1,1,1);
        int[][] b = w.getTiles();
        for(int i = 0; i < expectedB.length ; i++) for(int j = 0; j < expectedB[i].length; j++) assertEquals(expectedB[i][j], b[i][j]);
        for(int i = 0; i < expectedS.length ; i++) assertEquals(expectedS[i], s[i]);        
    }

    @Test
    public void walk2() {
        int[][] base = {
            {1,2,3,4},
            {5,10},
            {1,2,15},
            {0}
        };
        WalkingBoardWithPlayers w = new WalkingBoardWithPlayers(base,3);
        int[][] expectedB = {
            {8,7,6,3},
            {12,10},
            {13,3,15},
            {3}
        };
        int[] expectedS = {15,6,3};
        int[] s = w.walk(1,1,1,1,1,1,2,2,2,2);
        int[][] b = w.getTiles();
        for(int i = 0; i < expectedB.length ; i++) for(int j = 0; j < expectedB[i].length; j++) assertEquals(expectedB[i][j], b[i][j]);
        for(int i = 0; i < expectedS.length ; i++) assertEquals(expectedS[i], s[i]);        
    }
}