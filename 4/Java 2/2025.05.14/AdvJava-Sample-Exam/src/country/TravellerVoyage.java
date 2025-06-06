package country;

import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.concurrent.ThreadLocalRandom;
import java.util.function.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class TravellerVoyage {
    private static <E> E getRndElem(Set<E> set) {
        Random rnd = ThreadLocalRandom.current();
        return set.stream().skip(rnd.nextInt(0, set.size())).findFirst().orElseThrow();
    }


    public static void main(String[] args) {
        var data = Traveller.countryToBorderToSizes.get();
        Random rnd = ThreadLocalRandom.current();

        Supplier<List<String>> createRandomEntity = () -> {
            return IntStream.range(0, rnd.nextInt(2, 7))
                    .mapToObj(i -> getRndElem(data.keySet()))
                    .toList();
        };

        BiFunction<List<String>, List<String>, Stream<List<String>>> doCrossover = (v1, v2) -> {
            return Stream.of(Stream.concat(
                            v1.stream().limit(rnd.nextInt(1, v1.size() + 1)),
                            v2.stream().skip(rnd.nextInt(1, v2.size() + 1)))
                    .toList());
        };

        BiFunction<List<String>, Integer, List<String>> doMutate = (v, chance) -> {
            return v.stream()
                    .map(country -> rnd.nextInt(0, 1000) < chance ? getRndElem(data.keySet()) : country)
                    .toList();
        };

        BiFunction<List<String>, Integer, Integer> neighbourFitness = (v, i) ->
                -data.get(v.get(i)).getOrDefault(v.get(i + 1), 0);
        ToIntFunction<List<String>> getGoodFitness = (v) ->
                IntStream.range(0, v.size() - 1).map(i -> neighbourFitness.apply(v, i)).sum();
        ToIntFunction<List<String>> getRepeatCountryCount = (v) ->
                v.size() - new HashSet<>(v).size();
        ToIntFunction<List<String>> getPenalty = (v) ->
                500 * getRepeatCountryCount.applyAsInt(v) * (getRepeatCountryCount.applyAsInt(v) + 1);

        ToIntFunction<List<String>> fitnessValue = (v) ->
                getGoodFitness.applyAsInt(v) + getPenalty.applyAsInt(v);

        Consumer<List<String>> displayVoyage = (v) -> {
            IntStream.range(0, v.size() - 1).forEach(i -> System.out.print(v.get(i) + "-" + v.get(i + 1) + ":" + neighbourFitness.apply(v, i) + ", "));
            System.out.println(+v.size() + " countries, " + getRepeatCountryCount.applyAsInt(v) + " repeats, total: " + getGoodFitness.applyAsInt(v) + "+" + getPenalty.applyAsInt(v) + "=" + fitnessValue.applyAsInt(v));
        };

        var res = GeneticAlgorithm.genAlg(
                10,
                100,
                2,
                10,
                6,
                createRandomEntity,
                doCrossover,
                doMutate,
                fitnessValue
        );
        System.out.println(res);
        displayVoyage.accept(res);
    }
}
