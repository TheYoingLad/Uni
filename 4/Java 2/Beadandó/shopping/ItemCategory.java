package shopping;

public enum ItemCategory {
    ELECTRONICS,
    BABY_CARE,
    FOOD,
    ALCOHOL;

    public boolean isEdible() {
        return switch (this) {
            case FOOD, ALCOHOL -> true;
            default -> false;
        };
    }
}
