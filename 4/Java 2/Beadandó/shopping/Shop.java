package shopping;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;

public class Shop {
    private final Map<Item, Double> prices;
    private final Map<Item, Integer> stock;
    private final Object lock = new Object();

    public Shop() throws IOException {
        prices = new HashMap<>();
        stock = new HashMap<>();

        try (BufferedReader reader = new BufferedReader(new FileReader("warehouse.txt"))) {
            reader.lines().forEach(line -> {
                var itemInfo = ReaderUtil.readLine(line);
                stock.put(itemInfo.a(), 100);
                prices.put(itemInfo.a(), itemInfo.b());
            });
        }
    }

    public Map<Item, Integer> generateRandomShoppingList() {
        Random rand = ThreadLocalRandom.current();

//        List<Item> validItems = stock
//                .entrySet()
//                .stream()
//                .filter(e -> e.getValue() > 0)
//                .map(Map.Entry::getKey).toList();
//        if (validItems.isEmpty())
//            return Map.of(stock.keySet().stream().toList().get(rand.nextInt(0, stock.size())), rand.nextInt(1, 25)); // ha nincs semmi akkor mindegy miből de valamit kell választani
//        int itemCount = rand.nextInt(1, validItems.size());
//        List<Integer> chosenItems = new ArrayList<>();

        // nem kell synchronised, mert nem változtatunk a közös erőforráson

        int itemCount = rand.nextInt(1, stock.size() + 1);

        var items = new ArrayList<>(stock.keySet());
        Collections.shuffle(items, rand);

        return items.stream()
                .limit(itemCount)
                .collect(Collectors.toMap(item -> item, item -> rand.nextInt(1, 25)));
    }

    public void tryToBuy(Map<Item, Integer> shoppingList, Object customer) {
        synchronized (lock) { // stock.get miatt kell
            if (shoppingList.entrySet()
                    .stream()
                    .map(e -> stock.get(e.getKey()) - e.getValue())
                    .anyMatch(e -> e < 0)) {
                System.out.println("Purchase is not possible");
                return;
            }

            double[] cost = new double[1];
            cost[0] = 0;
            Set<ItemCategory> categories = new HashSet<>();

            shoppingList.forEach((item, db) -> {
                cost[0] += db * prices.get(item);
                categories.add(item.category());
            });

            boolean rejected = Arrays.stream(customer.getClass().getDeclaredMethods())
                    .filter(m -> m.isAnnotationPresent(ShoppingListValidator.class))
                    .anyMatch(m -> { // ha a stream üres akkor ez hamisat ad vissza
                        try {
                            return !(boolean) m.invoke(customer, categories.stream().toList(), cost[0]);
                        } catch (IllegalAccessException | InvocationTargetException e) {
                            throw new RuntimeException(e);
                        }
                    });

            if (rejected) {
                System.out.println("Rejected sell");
                return;
            }

            shoppingList.forEach((item, db) -> stock.computeIfPresent(item, (oldItem, oldDb) -> oldDb - db));
            System.out.println("Sell in " + categories + " for " + cost[0]);
        }
    }
}
