package text.to.numbers;

import java.io.*;

public class SingleLineFile{
    public static int addNumbers(String s) throws IOException{
        int sum = 0;
        try(BufferedReader br = new BufferedReader(new FileReader(s))){
            String line = br.readLine();
            PrintWriter pw = new PrintWriter("wrong."+s);
            if(line == null) throw new IllegalArgumentException("Empty file");
            String[] numbers = line.split(" ");
            for(String number : numbers){
                try{
                    sum += Integer.parseInt(number);
                } catch(NumberFormatException e){
                    System.err.println(number);
                    pw.println(number);
                }
            }
        }
        return sum;
    }
}