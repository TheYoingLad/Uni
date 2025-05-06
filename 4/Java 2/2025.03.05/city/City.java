package city;

public enum City {
    BUDAPEST(1000), LONDON(2000), PARIS(3000), FAKE(BUDAPEST), ECSER(1111);


    private final int zipCode;


    City(int zipCode) {
        this.zipCode = zipCode;
    }

    City(City other) {
        this(other.zipCode);
    }


    public int getZipCode() {
        return zipCode;
    }


    @Override
    public String toString() {
        return this.name() + ", " + zipCode;
        //StringBuilder megoldás
        //"%s(%d)".formatted(name(), zipcode);
    }


    public static void main(String[] args) {
        for (City c : City.values()) {
            System.out.println(c);
        }
    }
}