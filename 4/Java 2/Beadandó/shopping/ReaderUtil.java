package shopping;

import java.util.function.BiFunction;
import java.util.function.Function;

public class ReaderUtil {
    public static double readPrice(String string) {
        return Double.parseDouble(string);
    }

    public static Item readItem(String name, String category) {
        return new Item(name, ItemCategory.valueOf(category));
    }

    public static <A, B, C, D, E> TriFunction<A, B, D, Pair<C, E>> parsePair(BiFunction<A, B, C> f1, Function<D, E> f2) {
        return ((a, b, d) -> new Pair<>(f1.apply(a, b), f2.apply(d)));
    }

    public static Pair<Item, Double> readLine(String line) {
        String[] parts = line.split(",");
        return parsePair(ReaderUtil::readItem, ReaderUtil::readPrice).apply(parts[0], parts[1], parts[2]);
    }

    public static void main(String[] args) {
        Item i = readItem("alma", "FOOD");
        System.out.println(i.name());
        System.out.println(i.category());
    }
}
