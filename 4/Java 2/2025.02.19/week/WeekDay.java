package week;

import java.util.Arrays;
import java.util.Map;

public enum WeekDay {
    MON(Map.entry("EN", "Monday"), Map.entry("HU", "Hétfő")),
    TUE(Map.entry("HU", "Kedd")),
    WED(Map.entry("EN", "Wednesday"), Map.entry("HU", "Szerda")),
    THU(Map.entry("EN", "Thursday"), Map.entry("HU", "Csütörtök")),
    FRI(Map.entry("EN", "Friday")),
    SAT(Map.entry("EN", "Saturday"), Map.entry("HU", "Szombat")),
    SUN;


    public static final String[] supported = {"HU", "EN"};
    private final Map<String, String> names;


    private WeekDay(Map.Entry<String, String>... n) {
        names = Map.ofEntries(n);
    }


    public WeekDay nextDay(int i) {
        if (i == 0) return this;

        WeekDay[] d = values();
        return d[((ordinal() + i) % d.length + d.length) % d.length];
    }
    public WeekDay nextDay() {
        return nextDay(1);
    }


    public String get(String lang){
        for (String l : supported){
            if(lang.equals(l)) return names.get(l);
        }
        return "?";
    }


    public static void main(String[] args) {
        System.out.println(MON.name() + ", " + MON.nextDay(-1000).name());
    }
}
