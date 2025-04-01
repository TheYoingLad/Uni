package election.candidate;
public enum Candidate {
    JACK, JILL, SAM, MAX;

    public static int count(){
        return values().length;
    }
}