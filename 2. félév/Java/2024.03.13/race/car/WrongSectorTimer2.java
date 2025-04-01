package race.car;

public class WrongSectorTimer2{
    private int[] sectorTimes;
    
    public int[] getSectorTimes(){return sectorTimes;}
    public void setSectorTimes(int[] ns){sectorTimes = ns;}

    public WrongSectorTimer2(int[] ns){
        sectorTimes = ns;
    }

    public int getLapTime(int n){
        return 0;
    }

    public int getSectorTime(int n){
        return 0;
    }
}