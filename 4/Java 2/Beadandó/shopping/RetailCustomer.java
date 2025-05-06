package shopping;

import java.util.List;
import java.util.Random;

public class RetailCustomer {
    private final int budget;

    public RetailCustomer() {
        budget = new Random().nextInt(100, 1001);
    }

    @ShoppingListValidator
    boolean checkBudget(List<ItemCategory> categories, double price) {
        return price <= budget;
    }
}

