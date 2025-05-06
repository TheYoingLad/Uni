public class Fact{
    public static void main(String[] args){
        if(args.length != 1){
            System.out.println("1 argumentum kell!");
            return;
        }

        int fact = Integer.parseInt(args[0]);

        if(fact < 0){
            System.out.println("0-nál nagyobb szám kell!");
            return;
        }

        int n = 1;

        for(int i = 2; i <= fact; i++){
            n *= i;
        }

        System.out.println(fact + "! = " + n);
    }
}