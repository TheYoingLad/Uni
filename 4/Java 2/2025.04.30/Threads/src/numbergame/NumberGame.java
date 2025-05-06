package numbergame;

import java.util.*;
import java.util.concurrent.ThreadLocalRandom;

public class NumberGame {
    public static final Object lock = new Object();
    public static Map<Integer, OptionalInt> numbers = new HashMap<>();

    public static Thread getPlayer(int id) {
        return new Thread(() -> {
            while (true) {
                var random = ThreadLocalRandom.current();
                synchronized (lock) {
                    int randomNum = random.nextInt(1000001);
                    if (numbers.containsKey(randomNum)) {
                        numbers.put(randomNum, OptionalInt.empty());
                    } else {
                        numbers.put(randomNum, OptionalInt.of(id));
                    }
                }
            }
        });
    }

    private static Thread getServer() {
        return new Thread(() -> {
            while (true) {
                sleepFor1Sec();
                synchronized (lock) {
                    var res = numbers.entrySet().stream()
                            .filter(entry -> entry.getValue().isPresent())
                            .min(Map.Entry.comparingByKey())
                            .get();
                    System.out.println("Legkisebb szám: " + res.getKey() + "; Tippelő: " + res.getValue().getAsInt());
                    numbers.clear();
                }
            }
        });
    }

    private static void sleepFor1Sec() {
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }

    public static void main(String[] args) {
        Thread server = getServer();

        server.start();

        getPlayer(1).start();
        getPlayer(2).start();
        getPlayer(3).start();
    }
}
