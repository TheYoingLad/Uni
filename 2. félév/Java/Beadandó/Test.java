import walking.game.*;
import walking.game.util.*;
import walking.game.player.*;

public class Test{
    public static void main(String[] args){        
        while(true){
            System.out.println("1: test size with players");
            System.out.println("2: test custom with players");
            int n = Integer.parseInt(System.console().readLine());
            int player;
            switch (n){
                case 1:
                    System.out.println("size: ");
                    int size = Integer.parseInt(System.console().readLine());
                    System.out.println("players: ");
                    player = Integer.parseInt(System.console().readLine());
                    testSize(size, player);
                    break;
                case 2:
                    System.out.println("players: ");
                    player = Integer.parseInt(System.console().readLine());
                    testCustom(player);
                    break;
            }
        }
    }

    public static void testSize(int size, int player){
        System.out.println();
        WalkingBoardWithPlayers w = new WalkingBoardWithPlayers(size, player);
        int[] s = w.walk(1,1,1,1,1,1);
        int[][] b = w.getTiles();
        StringBuilder sb = new StringBuilder();
        for(int[] r:b) {
            for(int i = 0; i < r.length; i++) sb.append(r[i]).append(", ");
            sb.append("\n");
        }
        System.out.println(sb.toString());
        sb = new StringBuilder();
        for(int i = 0; i < s.length; i++) sb.append(s[i]).append(", ");
        System.out.println(sb.toString());
    }
    public static void testCustom(int player){
        System.out.println();
        int[][] test = {
            {1,2,3,4},
            {5,10,3},
            {1,2,15},
            {0}
        };
        WalkingBoardWithPlayers w = new WalkingBoardWithPlayers(test, player);
        int[] s = w.walk(1,1,1,1,1,1,2,2,2,2);
        int[][] b = w.getTiles();
        StringBuilder sb = new StringBuilder();
        for(int[] r:b) {
            for(int i = 0; i < r.length; i++) sb.append(r[i]).append(", ");
            sb.append("\n");
        }
        System.out.println(sb.toString());
        sb = new StringBuilder();
        for(int i = 0; i < s.length; i++) sb.append(s[i]).append(", ");
        System.out.println(sb.toString());
    }
}