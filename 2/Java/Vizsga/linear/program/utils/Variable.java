package linear.program.utils;

public class Variable{
    private int lowerBound;
    private int upperBound;
    private String name;
    private int value;

    private Variable(int l, int u, String n){
        if(u-l < 0) throw new IllegalArgumentException();
        lowerBound = l;
        upperBound = u;
        name = n;
    }

    public int getLowerBound(){return lowerBound;}
    public int getUpperBound(){return upperBound;}
    public String getName(){return name;}
    public int getValue(){return value;}
    public void setValue(int v){value = v;}

    public int getRange(){
        return upperBound-lowerBound+1;
    }
    public static Variable makeVar(int l, int u, String n){
        return new Variable(l, u, n);
    }
    @Override
    public String toString(){
        return name + ": " + value;
    }
}