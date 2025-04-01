public class Szamol{
    public static void main(String[] args){
        if(args.length != 2){
            System.out.println("2 argumentum kell!");
            return;
        }

        int elso = Integer.parseInt(args[0]);
        int masodik = Integer.parseInt(args[1]);;

        System.out.println("Sum: " + (elso + masodik));
        System.out.println("Diffrence: " + (elso - masodik));
        System.out.println("Product: " + (elso * masodik));
        if (masodik != 0){
            System.out.println("Quotient: " + (double)elso / masodik);
            System.out.println("Remainder: " + (elso % masodik));
        }
        else System.out.println("0-val nem lehet osztani");
    }
}