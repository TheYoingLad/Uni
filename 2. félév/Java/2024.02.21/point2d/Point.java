package point2d;

public class Point{
    private double x;
    private double y;
    public static final Point origin = new Point();

    public Point(double x, double y){
        this.x = x;
        this.y = y;
    }

    private Point(){
        this(0.0, 0.0);
    }

    public double getX(){
        return x;
    }

    public double getY(){
        return y;
    }

    public void move(double dx, double dy){
        x += dx;
        y += dy;
    }

    public void mirror(Point that){
        x += 2*(that.x - x);
        y += 2*(that.y - y);
    }

    public double distance(Point that){
        return Math.sqrt((x - that.x) * (x - that.x) + (y - that.y) * (y - that.y));
    }

    public String toString(){
        return "(" + x + "," + y + ")";
    }
}