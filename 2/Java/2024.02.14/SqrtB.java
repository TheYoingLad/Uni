public class SqrtB{
    public static void main(String[] args){
        if(args.length != 2){
            System.out.println("2 argumentum kell!");
            return;
        }
        
        double s = Double.parseDouble(args[0]);
        double e = Double.parseDouble(args[1]);
        double xn;
        double xnn = s / 2;

        do{
            xn = xnn;
            xnn = 0.5 * (xn + (s / xn));
        } while (Math.abs(xnn-xn) > e);
        
        System.out.println(s + " gyöke: " + xnn);
    }
}