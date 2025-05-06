package lambdas;

import java.util.HashMap;
import java.util.Map;
import java.util.function.BinaryOperator;

public class MapCombineLambda {
    public static BinaryOperator<Map<String,Integer>> mapCombine = (map1, map2) -> {
        Map<String, Integer> result = new HashMap<>(map1);
        for (var entry : map2.entrySet()) {
            result.merge(entry.getKey(), entry.getValue(), Integer::sum);
        }
        return result;
    };

    public static void main(String[] args) {
        Map<String, Integer> m1 = new HashMap<>(Map.of("alma", 2, "bela", 1));
        Map<String, Integer> m2 = new HashMap<>(Map.of("alma", 2, "peti", 3));

        System.out.println(mapCombine.apply(m1, m2));
    }
}
