public class SegmentMain{
    public static void main(String[] args){
        Segment s = new Segment(1,2,5,6);
        Point p1 = new Point(1,2);
        Point p2 = new Point(0,1);
        Point p3 = new Point(3,4);
        System.out.println(s.contains(p1));
        System.out.println(s.contains(p2));
        System.out.println(s.contains(p3));
        Point p4 = new Point(5,2);
        Point p5 = new Point(2,6);
        System.out.println(s.orientation(p3));
        System.out.println(s.orientation(p4));
        System.out.println(s.orientation(p3));
        System.out.println(s.line());
    }
}

class Segment{
    private double x1;
    private double y1;
    private double x2;
    private double y2;

    public Segment(double x1, double y1, double x2, double y2){
        this.x1 = x1;
        this.y1 = y1;
        this.x2 = x2;
        this.y2 = y2;
    }

    public Line line(){
        return new Line(y1-y2, x2-x1, x1*(y1-y2) + y1*(x2-x1));
    }

    public Point getP1(){
        return new Point(x1, y1);
    }

    public Point getP2(){
        return new Point(x2, y2);
    }

    public boolean contains(Point p){
        Line l = line();
        if(l.contains(p) && getP1().distance(p) + p.distance(getP2()) == getP1().distance(getP2())) return true;
        return false;
    }

    public double orientation(Point p){
        if(line().contains(p)) return 0;
        return (y2-y1)*(p.getX()-x2) - (p.getY()-y2)*(x2-x1);
    }

    public boolean intersects(Segment s){
        if(line().isParralel(s.line())) return contains(s.getP1()) || contains(s.getP2());
        return contains(line().intersect(s.line()));
    }
}