package walking.game;
import walking.game.util.Direction;
import walking.game.player.*;

public class WalkingBoardWithPlayers extends WalkingBoard{
    private Player[] players;
    private int round;
    public static final int SCORE_EACH_STEP = 13;

    public WalkingBoardWithPlayers(int[][] board, int playerCount){
        super(board);
        initPlayers(playerCount);
    }
    public WalkingBoardWithPlayers(int size, int playerCount){
        super(size);
        initPlayers(playerCount);
    }

    private void initPlayers(int playerCount){
        if(playerCount < 2) throw new IllegalArgumentException();
        players = new Player[playerCount];
        players[0] = new MadlyRotatingBuccaneer();
        for(int i = 1; i < playerCount; i++) players[i] = new Player();
    }
    public int[] walk(int... stepCounts){
        int currentPlayer = 0;
        int value = 0;
        int[] points = new int[players.length];
        for(int i = 0; i < stepCounts.length; i++){
            players[currentPlayer].turn();
            for(int j = 0; j < stepCounts[i]; j++){
                players[currentPlayer].addToScore(moveAndSet(players[currentPlayer].getDirection(), Math.min(value, SCORE_EACH_STEP)));
                value++;
            }
            currentPlayer = (currentPlayer+1) % players.length;
        }
        for(int i = 0; i < players.length; i++) points[i] = players[i].getScore();
        return points;
    }
}
