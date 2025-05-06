public class Perfect{
    public static void main(String[] args){
        if(args.length == 0){
            System.out.println("Kell argumentum!");
            return;
        }

        for(int i = 0; i < args.length; i++){
            int num = Integer.parseInt(args[i]);
            System.out.println("Perfect numbers between 1 and " + num + ":");
            for(int j = 1; j <= num; j++){
                int sum = 0;
                for(int k = 1; k < j; k++) if(j % k == 0) sum += k;
                if(sum == j) System.out.println(j);
            }
        }
    }
}