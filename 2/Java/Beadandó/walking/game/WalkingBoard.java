package walking.game;
import walking.game.util.Direction;

public class WalkingBoard{
    private int[][] tiles;
    private int x;
    private int y;
    public static final int BASE_TILE_SCORE = 3;

    public int[][] getTiles(){
        int[][] out = new int[tiles.length][];
        int i = 0;
        for(int[] row:tiles){
            out[i] = row.clone();
            i++;
        }
        return out;
    }
    public int[] getPosition(){return new int[] {x, y};}

    public WalkingBoard(int size){
        tiles = new int[size][size];
        for(int i = 0; i < size; i++) for(int j = 0; j < size; j++) tiles[i][j] = BASE_TILE_SCORE;
    }
    public WalkingBoard(int[][] tiles){
        this.tiles = new int[tiles.length][];
        int i = 0;
        for(int[] source:tiles){
            int[] row = new int[source.length];
            int j = 0;
            for(int tile:source){
                if(tile > BASE_TILE_SCORE) row[j] = tile;
                else row[j] = BASE_TILE_SCORE;
                j++;
            }
            this.tiles[i] = row;
            i++;
        }
    }

    public boolean isValidPosition(int x, int y){
        if(x < 0 || y < 0 || x > tiles.length-1 || y > tiles[x].length-1) return false;
        return true;
    }
    public int getTile(int x, int y){
        if(!isValidPosition(x, y)) throw new IllegalArgumentException();
        return tiles[x][y];
    }
    public static int getXStep(Direction direction){
        switch(direction){
            case UP:
                return -1;
            case DOWN:
                return 1;
            case LEFT:
                return 0;
            case RIGHT:
                return 0;
            default:
                return 0;
        }
    }
    public static int getYStep(Direction direction){
        switch(direction){
            case UP:
                return 0;
            case DOWN:
                return 0;
            case LEFT:
                return -1;
            case RIGHT:
                return 1;
            default:
                return 0;
        }
    }
    public int moveAndSet(Direction direction, int value){
        if(!isValidPosition(x+getXStep(direction), y+getYStep(direction))) return 0;
        x += getXStep(direction);
        y += getYStep(direction);
        int old = tiles[x][y];
        tiles[x][y] = value;
        return old;
    }
}