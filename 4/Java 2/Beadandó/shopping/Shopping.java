package shopping;

import java.io.IOException;
import java.util.Random;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadLocalRandom;

public class Shopping {
    public static void main(String[] args) throws IOException {
        Shop shop = new Shop();

        ExecutorService pool = Executors.newCachedThreadPool();

        for (int i = 0; i < 50; i++) {
            pool.execute(() -> {
                Random rand = ThreadLocalRandom.current();

                var shoppingList = shop.generateRandomShoppingList();

                shop.tryToBuy(shoppingList, switch (rand.nextInt(0, 3)) {
                    case 1 -> new RetailCustomer();
                    case 2 -> new NonRestrictedCustomer();
                    default -> new HumanCustomer();
                });
            });
        }

        pool.shutdown();
    }
}
