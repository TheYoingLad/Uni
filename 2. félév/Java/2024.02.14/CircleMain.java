public class CircleMain{
    public static void main(String[] args){
        Circle c1 = new Circle(0, 0, 1);
        c1.enlarge(5);
        System.out.println(c1);
        System.out.println(c1.getArea());
    }
}

class Circle{
    private double x;
    private double y;
    private double r;

    public Circle(double x, double y, double r){
        this.x = x;
        this.y = y;
        this.r = r;
    }

    public void enlarge(double f){
        r *= f;
    }

    public double getArea(){
        return r*r*Math.PI;
    }

    public String toString(){
        return "o: (" + x + "," + y + "), r: " + r;
    }
}