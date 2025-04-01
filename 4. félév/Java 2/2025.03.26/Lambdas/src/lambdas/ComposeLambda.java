package lambdas;

import java.util.function.BinaryOperator;
import java.util.function.IntUnaryOperator;

public class ComposeLambda {
    public static BinaryOperator<IntUnaryOperator> compose = (f, g) -> g.andThen(f);

    public static void main(String[] args) {
        IntUnaryOperator op1 = n-> n*2;
        IntUnaryOperator op2 = n-> n+1;

        System.out.println(compose.apply(op1, op2).applyAsInt(5));
        System.out.println(compose.apply(op2, op1).applyAsInt(5));
    }
}
