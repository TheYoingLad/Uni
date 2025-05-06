import java.util.*;
import java.util.stream.*;

public class Lab08{
    public static ArrayList<String> withSameStartEnd(ArrayList<String> source){
        ArrayList<String> result = new ArrayList<String>();
        for(String elem:source){
            if(elem != null && elem.trim().equals("") && elem.charAt(0) == elem.charAt(elem.length() - 1)) result.add(elem);
        }
        return result;
    }

    public static ArrayList<String> withSameStartEnd2(ArrayList<String> source){
        return source.stream()
                     .filter(elem -> elem != null && 
                                     !elem.trim().equals("") && 
                                     elem.charAt(0) == elem.charAt(elem.length() - 1))
                     .collect(Collectors.toCollection(ArrayList::new));
    }

    public static void maxToFront(ArrayList<String> source){
        String max = Collections.max(source);
        source.remove(max);
        source.add(0, max);
    }

    public static ArrayList<String> maxToFront2(ArrayList<String> source){
        String max = Collections.max(source);
        return Stream.concat(Stream.of(max), 
                             source.stream()
                                   .filter(e -> !e.equals(max)))
                     .collect(Collectors.toCollection(ArrayList::new));
    }
}