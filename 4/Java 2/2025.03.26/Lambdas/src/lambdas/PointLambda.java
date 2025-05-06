package lambdas;


import point.Point;

import java.util.Random;
import java.util.function.Supplier;

public class PointLambda {
    public static Supplier<Point> pointRandom = new Supplier<>() {
        Random r = new Random();
        @Override
        public Point get() {
            return new Point(r.nextInt(), r.nextInt());
        }
    };

    public static Supplier<Point> pointRow = new Supplier<>() {
        int limit_x = 2;
        int limit_y = 2;
        int x = -limit_x;
        int y = -limit_y;
        @Override
        public Point get() {
            if(x != limit_x){
                return new Point(x++, y);
            }
            if(y != limit_y) {
                return new Point(x, y++);
            }
            x = -limit_x;
            y = -limit_y;
            return new Point(x, y);
        }
    };

    public static Supplier<Point> pointSpiral = new Supplier<>() {
        int dir = 0;
        int limit_x = 1, limit_y = 1;
        int x = 0, y = 0;
        @Override
        public Point get() {
            switch (dir){
                case (0): {
                    if(x < limit_x) return new Point(x++, y);
                    dir = 1;
                    return new Point(x, y--);
                }
                case (1): {
                    if(-y < limit_y) return new Point(x, y--);
                    dir = 2;
                    return new Point(x--, y);
                }
                case (2): {
                    if(-x < limit_x) return new Point(x--, y);
                    dir = 3;
                    limit_x++;
                    return new Point(x, y++);
                }
                case (3): {
                    if(y < limit_x) return new Point(x++, y);
                    dir = 0;
                    limit_y++;
                    return new Point(x, y++);
                }
            }
            return new Point(0, 0);
        }
    };

    public static void main(String[] args) {
        System.out.println(pointRandom.get());
        System.out.println(pointRandom.get());
        System.out.println(pointRandom.get());

        System.out.println(pointRow.get());
        System.out.println(pointRow.get());
        System.out.println(pointRow.get());
        System.out.println(pointRow.get());

        System.out.println(pointSpiral.get());
        System.out.println(pointSpiral.get());
        System.out.println(pointSpiral.get());
        System.out.println(pointSpiral.get());
    }
}
