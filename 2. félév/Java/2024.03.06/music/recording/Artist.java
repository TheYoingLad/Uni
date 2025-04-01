package music.recording;

public class Artist{
    private final String name;
    private final RecordLabel label;

    public String getName(){return name;}
    public RecordLabel getLabel(){return label;}

    public Artist(String s, RecordLabel l){
        name = s;
        label = l;
    }
}