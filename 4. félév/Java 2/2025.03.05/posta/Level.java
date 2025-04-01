package posta;

import city.City;
import week.WeekDay;

import java.util.ArrayList;
import java.util.List;

public class Level {
    private final int zipCode;
    private List<City> ut;
    private WeekDay nap;

    public void utazik(City c){
        ut.add(c);
    }

    public int getZipCode() {
        return zipCode;
    }

    public void setNap(WeekDay nap) {
        this.nap = nap;
    }

    public Level(int zipCode, WeekDay nap) {
        this.zipCode = zipCode;
        ut = new ArrayList<>();
        this.nap = nap;
    }

    public Level(City city, WeekDay nap) {
        this.zipCode = city.getZipCode();
        ut = new ArrayList<>();
        this.nap = nap;
    }
}
