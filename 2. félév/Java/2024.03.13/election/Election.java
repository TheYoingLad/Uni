package election;
import election.candidate.Candidate;

public class Election{
    private int[] voteCounts;

    public Election(){
        voteCounts = new int[Candidate.count()];
    }

    public void addVote(Candidate c){
        voteCounts[c.ordinal()]++;
    }

    public void addVotes(Candidate c, int n){
        voteCounts[c.ordinal()] += n;
    }

    private int getCandidateCountWithMoreVotesThan(int n){
        return getCandidatesWithMoreVotesThan(n).length;
    }

    public Candidate[] getCandidatesWithMoreVotesThan(int n){
        return null;
    }

    public Candidate getWinner(){
        return null;
    }

    private int getWinningIdx(){
        return 0;
    }
}