package printed.material;

import java.io.*;

public class InvalidBookException throws IOException{
    private String author;
    private String title;

    public String getAuthor(){return author;}
    public String getTile(){return title;}

    public InvalidBookException(String a, String t){
        author = a;
        title = t;
    }
}