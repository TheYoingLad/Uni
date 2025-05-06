package plane.but.not.flying;
import plane.PublicCircle;
import plane.Circle;

class CircleMain{
    public static void main(String[] args){
        PublicCircle c = new PublicCircle();
        System.out.println(c.getArea());
        c.x = 5;
        c.y = 2;
        c.r = 10;
        System.out.println(c.getArea());

        Circle c2 = new Circle(0,0,1);
        System.out.println(c2.getArea());
        c2.setX(5);
        c2.setY(2);
        c2.setR(10);
        System.out.println(c2.getArea());
    }
}