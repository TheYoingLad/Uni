package filter;

import java.io.*;

public class FilterFile{
    public static void main(String[] args) {
        if(args.length < 2) throw new IllegalArgumentException();

        String path = args[0];
        String filter = args[1];

        try(BufferedReader br = new BufferedReader(new FileReader(path))){
            String line = br.readLine();
            PrintWriter pw = new PrintWriter("filter/out.txt");
            while(line != null){
                if(line.contains(filter)) {
                    System.out.println(line);
                    pw.println(line);
                }
                line = br.readLine();
            }
            pw.close();
        } catch(IOException e){
            e.printStackTrace();
        }
    }
}