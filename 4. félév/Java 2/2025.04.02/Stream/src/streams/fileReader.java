package streams;

import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class fileReader{
    public static Stream<String> readFromPath(){
        try {
            return Files.lines(Path.of("src", "streams", "input.txt"));
        } catch (IOException e) {
            return Stream.empty();
        }
    }

    public static void feladat1(){
        readFromPath().forEach(System.out::println);
    }
    public static void feladat2(){
        readFromPath()
                .map(line -> line + "alma")
                .forEach(System.out::println);
    }
    public static void feladat3(){
        try (PrintWriter writer = new PrintWriter("out1.txt")) {
            readFromPath()
                    .map(line -> line + "alma")
                    .forEach(writer::println);
        } catch (Exception e) {}
    }
    public static void feladat4(){
        try (PrintWriter writer = new PrintWriter("out2.txt")) {
            readFromPath()
                    .filter(line -> line.length() > 5)
                    .map(line -> line + "alma")
                    .forEach(writer::println);
        } catch (Exception e) {}
    }
    public static void feladat5(){
        try (PrintWriter writer = new PrintWriter("out3.txt")) {
            readFromPath()
                    .skip(3)
                    .map(line -> line + "alma")
                    .forEach(writer::println);
        } catch (Exception e) {}
    }
    public static void feladat6(){
        try (PrintWriter writer = new PrintWriter("out4.txt")) {
            readFromPath()
                    .limit(10)
                    .map(line -> line + "alma")
                    .forEach(writer::println);
        } catch (Exception e) {}
    }
    public static void feladat7(){
        try (PrintWriter writer = new PrintWriter("out5.txt")) {
            readFromPath()
                    .map(line -> line + "alma")
                    .limit(10)
                    .forEach(writer::println);
        } catch (Exception e) {}
    }
    public static void feladat8(){
        try (PrintWriter writer = new PrintWriter("out6.txt")) {
            readFromPath()
                    .map(line -> line + "alma")
                    .sorted()
                    .forEach(writer::println);
        } catch (Exception e) {}
    }
    public static void feladat9(){
        try (PrintWriter writer = new PrintWriter("out7.txt")) {
            readFromPath()
                    .map(line -> line + "alma")
                    .sorted(Comparator.comparingLong(s -> s.chars().distinct().count()))
                    .forEach(writer::println);
        } catch (Exception e) {}
    }
    public static void feladat10(){
        try (PrintWriter writer = new PrintWriter("out8.txt")) {
            readFromPath()
                    .map(s -> s + "alma")
                    .map(s -> {
                        List<Integer> e = s.chars().boxed().toList();
                        List<Integer> h = new ArrayList<>(e);
                        Collections.reverse(h);
                        if(e.equals(h)) return s;

                        return '"';
                    })
                    .forEach(writer::println);
        } catch (Exception e) {}
    }

//    … a palindrom szavakat úgy, ahogy vannak, a többieket “palindromosítva”: "abcd"-ből legyen "abcddcba"


    public static void main(String[] args) {
//        feladat1();
//        feladat2();
//        feladat3();
//        feladat4();
//        feladat5();
//        feladat6();
//        feladat7();
    }
}
