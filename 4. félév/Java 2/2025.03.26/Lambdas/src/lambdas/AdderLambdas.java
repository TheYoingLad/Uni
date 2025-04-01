package lambdas;

import java.util.function.IntSupplier;
import java.util.function.Supplier;

public class AdderLambdas {
    private static int[] state = {0};
    public static IntSupplier intsOutside = () -> ++state[0];

    public static IntSupplier intsInside = new IntSupplier() {
        private int state = 0;

        @Override
        public int getAsInt() {
            return ++state;
        }
    };

    public static Supplier<IntSupplier> intsFactory = () -> new IntSupplier() {
        private int state = 0;

        @Override
        public int getAsInt() {
            return ++state;
        }
    };
}