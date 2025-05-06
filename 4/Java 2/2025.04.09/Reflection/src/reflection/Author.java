package reflection;

import java.lang.annotation.*;
import java.util.Arrays;

@Target({ElementType.METHOD, ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@interface Authors {
    Author[] value();
}

@Target({ElementType.TYPE, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Repeatable(Authors.class)
public @interface Author {
    String[] value();
}

@Author("Jani")
@Author("Peti")
class Run {
    public static void printAuthor(String name) throws ClassNotFoundException {
        Class<?> clazz = Class.forName(name);
        if (clazz.isAnnotationPresent(Author.class))
            System.out.println(Arrays.toString(clazz.getAnnotation(Author.class).value()));
        if (clazz.isAnnotationPresent(Authors.class))
            System.out.println(Arrays.toString(Arrays.stream(clazz.getAnnotation(Authors.class).value())
                    .flatMap(a -> Arrays.stream(a.value()))
                    .toArray(String[]::new)));
    }

    public static void printDifferentAuthor(String name) throws ClassNotFoundException {
        Class<?> clazz = Class.forName(name);
        if (clazz.isAnnotationPresent(Author.class)) {
            String[] authors = clazz.getAnnotation(Author.class).value();
            Arrays.stream(clazz.getDeclaredMethods())
                    .filter(m -> m.isAnnotationPresent(Author.class))
                    .filter(m -> !Arrays.equals(m.getAnnotation(Author.class).value(), authors))
                    .forEach(m -> System.out.println(m.getName()));

            Arrays.stream(clazz.getDeclaredMethods())
                    .filter(m -> m.isAnnotationPresent(Authors.class))
                    .filter(m -> !Arrays.equals(Arrays.stream(m.getAnnotation(Authors.class).value())
                            .flatMap(a -> Arrays.stream(a.value()))
                            .toArray(String[]::new), authors))
                    .forEach(m -> System.out.println(m.getName()));
        }

        if (clazz.isAnnotationPresent(Authors.class)) {
            String[] authors = Arrays.stream(clazz.getAnnotation(Authors.class).value())
                    .flatMap(a -> Arrays.stream(a.value()))
                    .toArray(String[]::new);

            Arrays.stream(clazz.getDeclaredMethods())
                    .filter(m -> m.isAnnotationPresent(Author.class))
                    .filter(m -> !Arrays.equals(m.getAnnotation(Author.class).value(), authors))
                    .forEach(m -> System.out.println(m.getName()));

            Arrays.stream(clazz.getDeclaredMethods())
                    .filter(m -> m.isAnnotationPresent(Authors.class))
                    .filter(m -> !Arrays.equals(Arrays.stream(m.getAnnotation(Authors.class).value())
                            .flatMap(a -> Arrays.stream(a.value()))
                            .toArray(String[]::new), authors))
                    .forEach(m -> System.out.println(m.getName()));
        }
    }

    public static void main(String[] args) throws ClassNotFoundException {
        printAuthor("reflection.A");
        printDifferentAuthor("reflection.A");

        printAuthor("reflection.Run");

        printDifferentAuthor("reflection.B");
    }
}