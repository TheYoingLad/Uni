package exam.sinbad;
import exam.sinbad.sky.*;

import java.util.Arrays;
import java.util.List;
import java.util.ArrayList;

public class Adventure{
    protected int collectedDiamonds = 0;
    protected List<Bird> birds;
    protected int day = 1;
    protected static int storedDiamonds = 0;


    public Adventure(Bird ... inBirds){
        birds = new ArrayList<>();
        birds.addAll(Arrays.asList(inBirds));
    }
    public Adventure(){
        this(new Ankaa("BigBird", 700));
    }


    public int getCollectedDiamonds(){
        return collectedDiamonds;
    }
    public List<Bird> getBirds(){
        return new ArrayList<>(birds);
    }
    public int getDay(){
        return day;
    }
    public static int getStoredDiamonds(){
        return storedDiamonds;
    }


    public int getBirdCount(){
        return birds.size();
    }
    public void collectDiamonds(int diamondCount){
        collectedDiamonds += diamondCount;
    }
    protected void storeDiamonds(int diamondCount){
        storedDiamonds += diamondCount;
    }
    public void clearStoredDiamonds(){
        storedDiamonds = 0;
    }
    public boolean callBird(int height){
        for(Bird b : birds){
            if(b.isAtHeightRange(height)){
                doEscape(b);
                return true;
            }
        }
        return false;
    }
    protected void doEscape(Bird b){
        storeDiamonds(collectedDiamonds);
        collectedDiamonds = 0;
        day++;
    }
}