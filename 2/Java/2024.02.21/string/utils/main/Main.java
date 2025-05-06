package string.utils.main;
import string.utils.IterLetter;

public class Main{
    public static void main(String[] args){
        IterLetter s = new IterLetter(args[1]);
        for(int i = 0; i < Integer.parseInt(args[0]); i++) {
            if(i == Integer.parseInt(args[2])){
                s.reset();
                System.out.println("jaj");
                for(int j = 0; j <= Integer.parseInt(args[3]); j++) s.printNext();
            }
            s.printNext();
        }
    }
}