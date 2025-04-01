package walking.game.player;
import walking.game.util.Direction;

public class Player{
    private int score;
    protected Direction direction = Direction.UP;

    public int getScore(){return score;}
    public Direction getDirection(){return direction;}

    public Player(){}

    public void addToScore(int score){this.score += score;}
    public void turn(){
        switch (direction){
            case UP:
                direction = Direction.RIGHT;
                break;
            case RIGHT:
                direction = Direction.DOWN;
                break;
            case DOWN:
                direction = Direction.LEFT;
                break;
            case LEFT:
                direction = Direction.UP;
                break;
        }
    }
}
