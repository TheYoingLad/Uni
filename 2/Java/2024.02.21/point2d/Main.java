package point2d;

class Main{
    public static void main(String[] args){
        Point p = new Point(1,2);
        System.out.println(p);
        p.move(2,2);
        System.out.println(p);
        Point p2 = new Point(0,0);
        System.out.println("distance of " + p + " and " + p2 + " is " + p.distance(p2)); 
    }
}