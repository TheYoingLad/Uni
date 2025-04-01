package lambdas;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;
import java.util.function.UnaryOperator;

public class InseptionLambda <T> {
    //Készíts olyan lambdát, ami egy Supplier-t kap meg, és egy Supplier-t ad eredményül.
    // A kiadott Supplier először a megkapott Supplier első elemét, aztán annak az első két elemét, aztán az első három elemét stb. adja ki.
    // Példa: ha a megkapott Supplier az a, b, c, … szövegeket állítja elő, akkor a kiadott Supplier az a, a, b, a, b, c, … értékeket állítsa elő.

    public UnaryOperator<Supplier<T>> inseption = tSupplier -> {
        return new Supplier<T>() {
            List<T> elems = new ArrayList<>();
            int n = 0;
            @Override
            public T get() {
                if (n == elems.size()) {
                    elems.add(tSupplier.get());
                    n = 0;
                }
                return elems.get(n++);
            }
        };
    };

    public static void main(String[] args) {
        InseptionLambda<Integer> test = new InseptionLambda<>();
        Supplier<Integer> input = new Supplier<Integer>() {
            int n = 0;
            @Override
            public Integer get() {
                return n++;
            }
        };
        Supplier<Integer> output = test.inseption.apply(input);

        System.out.println(output.get());
        System.out.println(output.get());
        System.out.println(output.get());
        System.out.println(output.get());
        System.out.println(output.get());
        System.out.println(output.get());
    }
}