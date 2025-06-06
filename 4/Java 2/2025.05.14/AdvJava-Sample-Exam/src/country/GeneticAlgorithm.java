package country;

import java.util.Comparator;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import java.util.function.BiFunction;
import java.util.function.IntFunction;
import java.util.function.Supplier;
import java.util.function.ToIntFunction;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class GeneticAlgorithm {
    public static <Entity> Entity genAlg(
            int popCount,
            int iterCount,
            int crossoverCount,
            int mutProb,
            int pruneCount,
            Supplier<Entity> createRandomEntity,
//		BiFunction<Entity, Entity, Entity> doCrossover,
//		BiFunction<Entity, Entity, List<Entity>> doCrossover,
            BiFunction<Entity, Entity, Stream<Entity>> doCrossover,
            BiFunction<Entity, Integer, Entity> doMutate,
            ToIntFunction<Entity> getFitness
    ) {
        IntFunction<List<Entity>> makeEntities = n ->
                IntStream.range(0, n)
                        .mapToObj(i -> createRandomEntity.get())
                        .toList();
//			.collect(Collectors.toList());

        var rndGen = ThreadLocalRandom.current();
        List<Entity> pop = makeEntities.apply(popCount);

        for (int i = 0; i < iterCount; i++) {
            var pop2 = pop;
            pop = Stream.concat(
                    Stream.concat(
                                    //				pop2.stream(),
                                    pop2.stream().map(e -> doMutate.apply(e, mutProb)),

                                    IntStream.range(0, crossoverCount)
                                            .boxed()
                                            .flatMap(i2 -> {
                                                var idx1 = rndGen.nextInt(pop2.size());
                                                var idx2 = rndGen.nextInt(pop2.size());
                                                return doCrossover.apply(pop2.get(idx1), pop2.get(idx2));
                                            })
                                    // Stream<Entity>
                            )
                            .sorted(Comparator.comparingInt(getFitness))
                            .limit(pruneCount),

                    makeEntities.apply(popCount - pruneCount).stream()
            ).toList();
        }

        return pop.get(0);
    }
}
