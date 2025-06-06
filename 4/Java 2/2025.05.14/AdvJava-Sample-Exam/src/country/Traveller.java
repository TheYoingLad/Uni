package country;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;

public class Traveller {
    public static Supplier<Map<String, Map<String, Integer>>> countryToBorderToSizes = () -> {
        try (var data = Files.lines(Path.of("src", "country_borders.txt"))) {
            return data.map(l -> l.split(";")).collect(Collectors.toMap(
                    strings -> strings[0],
                    strings -> Arrays.stream(strings)
                            .skip(1)
                            .collect(Collectors.toMap(
                                    s -> s.split(":")[0],
                                    s -> Integer.parseInt(s.split(":")[1])))
            ));
        } catch (IOException e) {
            return new HashMap<>();
        }
    };

    public static Function<Map<String, Map<String, Integer>>, List<String>> getIsolatedCountries = data -> {
        return data.entrySet().stream().
                filter(e -> e.getValue().isEmpty())
                .map(Map.Entry::getKey)
                .collect(Collectors.toList());
    };

    public static Supplier<Map<String, Map<String, Integer>>> countryToBorderToSizes2 = () -> {
        return ignoreException(
                () -> {
                    return new HashMap<>();
                },
                () -> {
                    var data = Files.lines(Path.of("src", "country_borders.txt"));
                    var res = data.map(l -> l.split(";")).collect(Collectors.toMap(
                            strings -> strings[0],
                            strings -> Arrays.stream(strings)
                                    .skip(1)
                                    .collect(Collectors.toMap(
                                            s -> s.split(":")[0],
                                            s -> Integer.parseInt(s.split(":")[1])))
                    ));
                    data.close();
                    return res;
                });
    };

    static <T> T ignoreException(Supplier<T> defVal, ThrowingFunctionalInterface<T> val) {
        try {
            return val.get();
        } catch (Exception e) {
            System.err.println(Arrays.toString(e.getStackTrace()));
            return defVal.get();
        }
    }




    public static void main(String[] args) {
        System.out.println(countryToBorderToSizes2.get());
        System.out.println(getIsolatedCountries.apply(countryToBorderToSizes.get()));
        System.out.println(getIsolatedCountries.apply(countryToBorderToSizes.get()).size());
    }
}

@FunctionalInterface
interface ThrowingFunctionalInterface<T> {
    T get() throws Exception;
}