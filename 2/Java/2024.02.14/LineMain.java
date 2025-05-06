public class LineMain{
    public static void main(String[] args){
        Line l1 = new Line(-1,1,1);
        Line l2 = new Line(5,-5,30);
        Line l3 = new Line(5,-4,30);
        System.out.println(l1);
        System.out.println(l1.isParralel(l2));
        System.out.println(l1.isParralel(l3));
        Line l4 = new Line(1,1,2);
        System.out.println(l1.intersect(l4));
    }
}

class Line{
    private double a;
    private double b;
    private double c;

    public Line(double a, double b, double c){
        this.a = a;
        this.b = b;
        this.c = c;
    }

    public boolean contains(Point p){
        if(a*p.getX() + b*p.getY() == c) return true;
        return false;
    }

    public boolean isParralel(Line l){
        if((a / l.a) == (b / l.b)) return true;
        return false;
    }

    public boolean isOrthogonal(Line l){
        if((a / l.a) == -(l.b / b)) return true;
        return false;
    }

    public Point intersect(Line l){
        if(a+l.a == 0){
            if(b+l.b == 0) return new Point(0,0);
            return new Point(((c - (b*(c+l.c))/(b+l.b))/(a)),((c+l.c)/(b+l.b)));
        }
        return new Point((c+l.c - ((b+l.b)*(c*l.a - a*l.c))/(b*l.a - a*l.b))/(a+l.a), (c*l.a - a*l.c)/(b*l.a - a*l.b));
    }

    public String toString(){
        return a + "x + " + b + "y = " + c;
    }
}