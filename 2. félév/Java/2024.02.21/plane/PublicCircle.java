package plane;

public class PublicCircle{
    public double x;
    public double y;
    public double r;

    public PublicCircle(){
        x = 0;
        y = 0;
        r = 1;
    }

    public double getArea(){
        return r * r * Math.PI;
    }
}