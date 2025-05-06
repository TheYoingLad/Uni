package music.recording;

public class RecordLabel{
    private final String name;
    private int cash;

    public String getName(){return name;}
    public int getCash(){return cash;}

    public RecordLabel(String s, int n){
        name = s;
        cash = n;
    }

    public void gotIncome(int n){
        cash += n;
    }
}