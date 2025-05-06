package genetic;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Random;
import java.util.function.*;

public class GeneticAlgorithm {
    public static <Entity> Entity algorithm(int populationCount, Supplier<Entity> createRandomEntity,
                                            int crossoverCount, BinaryOperator<Entity> doCrossover,
                                            double mutationProbability, BiFunction<Entity, Double, Entity> mutateEntity,
                                            Function<Entity, Integer> calculateFitness,
                                            int pruneCount,
                                            int generationCount) {

        // init
        List<Entity> population = new ArrayList<>();
        for (int i = 0; i < populationCount; i++) population.add(createRandomEntity.get());
        Random r = new Random();

        // generation
        for (int i = 0; i < generationCount; i++) {
            // crossover
            for (int j = 0; j < crossoverCount; j++) {
                Entity e1 = population.get(r.nextInt(population.size()));
                Entity e2 = population.get(r.nextInt(population.size()));
                population.add(doCrossover.apply(e1, e2));
            }

            // mutation
            for (Entity e : population) mutateEntity.apply(e, mutationProbability);

            // sorting by fitness
            population.sort(Comparator.comparingInt(calculateFitness::apply).reversed());

            // prune
            population = population.subList(0, pruneCount - 1);

            // replace
            for (int j = 0; j < pruneCount; j++) population.add(createRandomEntity.get());
        }

        return population.get(0);
    }

    public static void main(String[] args) {

    }
}
