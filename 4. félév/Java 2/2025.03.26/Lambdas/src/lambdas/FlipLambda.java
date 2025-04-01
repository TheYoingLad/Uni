package lambdas;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

public class FlipLambda <T, E, R> {
    public Function<BiFunction<T, E, R>, BiFunction<E, T, R>> flipper =biFunction -> (e, t) -> biFunction.apply(t, e);

    public static void main(String[] args) {
        FlipLambda<String, Integer, List<String>> test = new FlipLambda<>();

        BiFunction<String, Integer, List<String>> input = (s, n) -> {
            List<String> l = new ArrayList<>();
            for (int i = 0; i < n; i++) l.add(s);
            return l;
        };

        var output = test.flipper.apply(input);

        System.out.println(output.apply(2, "alma").toString());
        System.out.println(output.apply(1, "beka").toString());
        System.out.println(output.apply(4, "huha").toString());
    }
}
