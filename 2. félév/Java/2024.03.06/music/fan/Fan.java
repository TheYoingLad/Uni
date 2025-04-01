package music.fan;
import music.recording.Artist;
public class Fan{
    private final String name;
    private final Artist favourite;
    private int moneySpent;

    public String getName(){return name;}
    public Artist getFavourite(){return favourite;}
    public int getMoneySpent(){return moneySpent;}

    public Fan(String s, Artist a){
        name = s;
        favourite = a;
        moneySpent = 0;
    }

    public int buyMerchandise(int n, Fan... friends){
        if(friends.length == 0){
            moneySpent += n;
            favourite.getLabel().gotIncome(n/2);
            return n;
        }
        if(friends.length == 1){
            moneySpent += (n*90)/100;
            friends[0].moneySpent += (n*90)/100;
            favourite.getLabel().gotIncome((n*90)/200);
            friends[0].favourite.getLabel().gotIncome((n*90)/200);
            return (n*90)/100;
        }
        moneySpent += (n*80)/100;
        for(int i = 0; i < friends.length; i++){
            friends[i].moneySpent += (n*80)/100;
            friends[i].favourite.getLabel().gotIncome((n*80)/200);
        }
        favourite.getLabel().gotIncome((n*80)/200);
        return (n*80)/100;
    }

    public boolean favesAtSameLabel(Fan f){
        return favourite.getLabel() == f.getFavourite().getLabel();
    }
    
    public String toString1(){
        return "Name: " + name + "\nFavourite: " + favourite + "\nMoney Spent: " + moneySpent;
    }
    public String toString2(){
        return "Name: %s\nFavourite: %s\nMoney Spent: %d".formatted(name, favourite, moneySpent);
    }
    public String toString3(){
        return String.format("Name: %s\nFavourite: %s\nMoney Spent: %d", name, favourite, moneySpent);
    }
    public String toString4(){
        StringBuilder sb = new StringBuilder();
        sb.append("Name: ");
        sb.append(name);
        sb.append("\nFavourite: ");
        sb.append(favourite);
        sb.append("\nMoney Spent: ");
        sb.append(moneySpent);
        return sb.toString();
    }
}