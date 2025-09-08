package alma.fa.bigyusz.cucc;

import java.util.function.IntConsumer;

public record Card(Szin szin, Ertek ertek) {
    public static IntConsumer alma = i -> System.out.println(i + 1);

    public Card(String str) {
        this(Szin.valueOf(str.split(",")[0]), Ertek.valueOf(str.split(",")[1]));
    }
}
