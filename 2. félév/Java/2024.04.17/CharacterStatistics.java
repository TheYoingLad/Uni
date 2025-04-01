import java.util.*;
import java.util.stream.*;

public class CharacterStatistics{
    private HashMap<Character, Integer> charToCount;

    public CharacterStatistics(String input){
        charToCount = new HashMap<Character, Integer>();
        for(char c:input.toCharArray()){
            if(charToCount.containsKey(c)) charToCount.put(c, charToCount.get(c)+1);
            else charToCount.put(c, 1);
        }
    }

    public int getCount(char c){
        return charToCount.getOrDefault(c, 0);
    }

    @Override
    public String toString(){
        return charToCount.entrySet().stream().map();
    }

    public static void main(String[] args){
        CharacterStatistics a = new CharacterStatistics("aaaaaaaa=,");
        System.out.println(a);
    }
}
