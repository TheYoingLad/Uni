package reflection;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Arrays;

@Author("A")
class A {
    public int a;
    private int b;

    @Author("nummy")
    public int num() {
        return 3;
    }

    @Author({"A", "B", "C"})
    public int num2() {
        return 10;
    }

    @Author("A")
    public int numGood() {
        return 19187263;
    }

    public int numEmpty() {
        return 0;
    }
}

@Author("Bela")
@Author("Jani")
class B {
    private String c;
    private int d;

    @Author({"Bela", "Jani"})
    public int num2() {
        return 10;
    }

    @Author("Bela")
    @Author("Jani")
    public int numGood() {
        return 19187263;
    }

    public int numEmpty() {
        return 0;
    }
}

public class AllPrivate {
    public static <T> boolean areAllFieldsPrivate(T type) {
        return Arrays.stream(type.getClass().getDeclaredFields())
                .map(Field::getModifiers)
                .allMatch(Modifier::isPrivate);
    }

    public static void main(String[] args) {
        System.out.println(areAllFieldsPrivate(new A()));
        System.out.println(areAllFieldsPrivate(new B()));
    }
}
