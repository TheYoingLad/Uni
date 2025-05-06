package shopping;

import java.util.List;
import java.util.Random;

public class HumanCustomer {
    private final int age;

    public HumanCustomer() {
        age = new Random().nextInt(12, 68);
    }

    @ShoppingListValidator
    boolean canIBuy(List<ItemCategory> categories, double price) {
        return age >= 18 || !categories.contains(ItemCategory.ALCOHOL);
    }
}
