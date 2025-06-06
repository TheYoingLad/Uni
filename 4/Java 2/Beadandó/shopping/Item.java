package shopping;

public record Item(String name, ItemCategory category) {
    public boolean isHealthy(){
        return switch (category){
            case ALCOHOL -> false;
            case FOOD -> true;
            default -> throw new IllegalStateException("Not supported question for: " + category);
        };
    }
}
