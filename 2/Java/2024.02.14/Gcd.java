public class Gcd{
    public static void main(String[] args){
        if(args.length < 2){
            System.out.println("Kell legalább 2 argumentum!");
            return;
        }
        
        long n1 = Long.parseLong(args[0]);

        for(int i = 1; i < args.length; i++){
            long n2 = Long.parseLong(args[i]);
            while (n2 != 0){
                long seged = n1 % n2;
                n1 = n2;
                n2 = seged;
            }
        }
        System.out.println("Ezek legnagyobb közös osztója: " + n1);
    }
}