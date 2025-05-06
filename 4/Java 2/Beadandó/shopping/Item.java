package shopping;

public record Item(String name, ItemCategory category) {
//    public Item(String name, ItemCategory category) {
//        this.name = name;
//        this.category = category;
//    }
//
//    public String getName() {
//        return name;
//    }
//
//    public ItemCategory getCategory() {
//        return category;
//    }

    public boolean isHealthy(){
        return switch (category){
            case ALCOHOL -> false;
            case FOOD -> true;
            default -> throw new IllegalStateException("Not supported question for: " + category);
        };
    }
}
