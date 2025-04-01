package exam.sinbad.sky;

public enum HeightRange{
    LOW(0,300), MEDIUM(301, 600), HIGH(601, 900), BEYOND;

    
    private int min;
    private int max;


    private HeightRange(int inMin, int inMax){
        min = inMin;
        max = inMax;
    }
    private HeightRange(){
        this(0, 0);
    }


    public static HeightRange getHeightRange(int h){
        for(HeightRange height : HeightRange.values()){
            if(height == HeightRange.BEYOND) break;
            if(height.min <= h && h <= height.max) return height;
        }
        return HeightRange.BEYOND;
    }
}