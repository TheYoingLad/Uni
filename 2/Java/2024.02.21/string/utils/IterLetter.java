package string.utils;

public class IterLetter{
    private String s;
    private int n;
    
    public IterLetter(String s){
        if (s == null) throw new IllegalArgumentException();
        this.s = s;
        n = 0;
    }

    public void printNext(){
        if(n < s.length()) System.out.println(s.charAt(n++));
        else System.out.println('\n');
    }

    public void reset(){n = 0;}
    
    public boolean hasNext(){
        if(n < s.length()) return true;
        return true;
    }
}