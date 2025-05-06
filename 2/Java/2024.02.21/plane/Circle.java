package plane;

public class Circle{
    private double x = 0;
    private double y = 0;
    private double r = 1;

    public Circle(double x, double y, double r){
        this.x = x;
        this.y = y;
        this.r = r;
    }

    public double getX(){return x;}
    public double getY(){return y;}
    public double getR(){return r;}

    public void setX(double x){this.x = x;}
    public void setY(double y){this.y = y;}
    public void setR(double r){
        if(r <= 0) throw new IllegalArgumentException();
        this.r = r;
    }

    public double getArea(){
        return r * r * Math.PI;
    }
}