package text.to.numbers;

import java.io.*;

public class MultiLineFile{
    public static int addNumbers(String s, char c) throws IOException{
        int sum = 0;
        try(BufferedReader br = new BufferedReader(new FileReader(s))){
            String line = br.readLine();
            while(line != null){
                String[] numbers = line.split(String.valueOf(c));
                for(String number : numbers){
                    try{
                        sum += Integer.parseInt(number);
                    } catch(NumberFormatException e){
                        System.err.println(number);
                    }
                }
                line = br.readLine();
            }            
        }
        return sum;
    }
}