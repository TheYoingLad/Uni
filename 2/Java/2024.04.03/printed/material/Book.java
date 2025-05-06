package printed.material;

public class Book extends Object{
    public static final String defaultAuthor = "Kozsik Tamás";
    public static final String defaultTitle = "Java programozás";
    public static final int defaultPageCount = 234;
    private String author;
    private String title;
    protected int pageCount;

    public String getAuthor(){return author;}
    public String getTitle(){return title;}
    public int getPageCount(){return pageCount;}

    public Book(){
        author = defaultAuthor;
        title = defaultTitle;
        pageCount = defaultPageCount;
    }
    public Book(String a, String t, int p){
        checkInitData(author, title, pageCount);
        author = a;
        title = t;
        pageCount = p;
    }

    private void checkInitData(String a, String t, int p){
        if(a.length() < 2 || t.length() < 4 || p < 0){
            throw new IllegalArgumentException();
        }
    }
    public String createReference (String a, int n, int m){return null;}
    public static Book decode(String s){
        String[] authorRest = s.split(":");
        String[] titlePageCount = authorRest[1].split(";");
        return new Book(authorRest[0], titlePageCount[0].strip(), Integer.parseInt(titlePageCount[1].strip()));
        }
    protected String getAuthorWithInitials(){
        StringBuilder sb = new StringBuilder();
        String[] authorNames = author.split(" ");
        for(int i = 0; i < authorNames.length - 1; i++){
            sb.append(authorNames[i].charAt(0)).append(". ");
        }
        sb.append(authorNames[authorNames.length - 1]);
        return sb.toString();
        }
    public int getPrice(){return pageCount;}
    public String getShortInfo(){
        StringBuilder sb = new StringBuilder();
        sb.append(getAuthorWithInitials()).append(": ").append(title.substring(0, 4)).append("; ").append(pageCount);
        return sb.toString();
        }
    protected void initBook(String a, String t, int p){}

    @Override
    public String toString(){return super.toString();}
}