public class Distance{
    public static void main(String[] args){
        double d = 0;
        if (args.length > 2){
            for (int i = 0; i < args.length / 2 - 1; i++){
                Point p1 = new Point(Double.parseDouble(args[2*i]), Double.parseDouble(args[2*i+1]));
                Point p2 = new Point(Double.parseDouble(args[2*i+2]), Double.parseDouble(args[2*i+3]));
                d += p1.distance(p2);
            }
        }
        System.out.println(d);
    }
}