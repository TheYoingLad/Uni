package famous.sequence;

public class TriangularNumbers{
    public static int getTriangularNumber(int n){
        if(n < 1) return 0;
        return (n*(n+1))/2;
    }

    public static int getTriangularNumberAlternative(int n){
        int s = 0;
        for(int i = 1; i <= n; i++) s += i;
        return s;
    }
}