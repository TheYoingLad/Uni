package exam.sinbad.sky;
import java.util.Objects;

public class Ankaa implements Bird{
    private String name;
    private int height;


    public Ankaa(String name, int height) {
        this.name = name;
        this.height = height;
    }
    public Ankaa(String name) {
        this.name = name;
    }

    public void setHeight(int h){
        height = h;
    }
    public String getName(){
        return name;
    }
    public int getHeight(){
        return height;
    }
    public boolean isAtHeightRange(int h){
        return HeightRange.getHeightRange(h) == HeightRange.getHeightRange(height);
    }


    
    @Override
    public String toString(){
        HeightRange h = HeightRange.getHeightRange(height);
        String helper;
        if(h == HeightRange.BEYOND) helper = "OneDoesNotSimplyFlyOutOfDiamondIsland";
        else helper = name;
        return helper + "[flying " + h.toString() +  " at " + height + " meters]";
    }
    @Override
    public boolean equals(Object obj){
        if(obj == null || getClass() != obj.getClass()) return false;
        Ankaa other = (Ankaa)obj;
        return HeightRange.getHeightRange(height) == HeightRange.getHeightRange(other.height) && name == other.name;
    }
    @Override
    public int hashCode(){
        return Objects.hash(name, HeightRange.getHeightRange(height));
    }
}