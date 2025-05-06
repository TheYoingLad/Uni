package genetic;

import java.util.List;
import java.util.Random;
import java.util.function.BinaryOperator;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

public class Code {
    public static int populationCount = 1_000_000;

    public static Supplier<Code> createRandomEntity = () -> {
        Random r = new Random();
        StringBuilder sb = new StringBuilder();

        for (int i = 0; i < 6; i++) sb.append((char) ('0' + r.nextInt(10)));
        return new Code(sb.toString());
    };

    public static int crossoverCount = 1_000;

    public static BinaryOperator<Code> doCrossover = ((c1, c2) -> new Code(c1.genes.substring(0,3) + c2.genes.substring(3,6)));

    public static double mutationProbability = 0.001;

    public static Consumer<Code> mutateEntity;

    public static Function<Code, Integer> calculateFitness;

    public static int pruneCount = 10_000;

    public static int generationCount = 100;

    public Code(String genes) {
        this.genes = genes;
    }

    public String genes; // code
}
