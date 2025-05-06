package posta;

import city.City;
import week.WeekDay;

public class Level {
    private final int zipCode;
    private WeekDay nap;

    public Level(int zipCode) {
        this.zipCode = zipCode;
    }

    public Level(City city) {
        this.zipCode = city.getZipCode();
    }


    public void erkezik(){
        nap = nap.nextDay();
    }
}
