package walking.game.player;
import walking.game.util.Direction;

public class MadlyRotatingBuccaneer extends Player{
    private int round;
    @Override
    public void turn(){
        for(int i = 0; i < round; i++) super.turn();        
        round++;
    }
}
