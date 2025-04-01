public class Felez{
    public static void main(String[] args){
        int elso;
        int masodik;

        System.out.println("Első szám: ");
        elso = Integer.parseInt(System.console().readLine());
        
        System.out.println("Második szám: ");
        masodik = Integer.parseInt(System.console().readLine());
        
        for(int i = elso + 1; i < masodik; i++){
            System.out.println(i + " fele " + i/2d);
        }
    }
}