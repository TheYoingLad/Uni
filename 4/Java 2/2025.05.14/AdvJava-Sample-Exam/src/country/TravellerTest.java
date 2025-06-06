package country;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

import java.util.*;
import java.util.function.ToIntBiFunction;

import static org.junit.jupiter.api.Assertions.*;

public class TravellerTest {
    @Test
    public void test() {
        var data = Traveller.countryToBorderToSizes.get().get("Hungary");
        var expected = Map.of("Austria", 366, "Croatia", 329, "Romania", 443, "Serbia", 151, "Slovakia", 677, "Slovenia", 102, "Ukraine", 103);

        assertEquals(expected, data);
    }

    @Test
    public void testIsolated() {
        var data = Traveller.getIsolatedCountries.apply(Traveller.countryToBorderToSizes.get());
        data.sort(String::compareTo);
        var expected = List.of("Antigua and Barbuda", "Australia", "Bahamas", "Bahrain", "Barbados", "Cape Verde", "Comoros", "Cuba", "Dominica", "Federated States of Micronesia", "Fiji", "Grenada", "Iceland", "Jamaica", "Japan", "Kiribati", "Madagascar", "Maldives", "Malta", "Marshall Islands", "Mauritius", "Nauru", "New Zealand", "Palau", "Philippines", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Samoa", "Seychelles", "Singapore", "Solomon Islands", "Sri Lanka", "São Tomé and Príncipe", "Tonga", "Trinidad and Tobago", "Tuvalu", "Vanuatu");

        assertEquals(expected, data);
    }

    @Test
    public void test2() {
        var data = Traveller.countryToBorderToSizes2.get().get("Hungary");
        var expected = Map.of("Austria", 366, "Croatia", 329, "Romania", 443, "Serbia", 151, "Slovakia", 677, "Slovenia", 102, "Ukraine", 103);

        assertEquals(expected, data);
    }

    @Test
    public void testSymmetry() {
        var data = Traveller.countryToBorderToSizes.get();

        ToIntBiFunction<String, String> distance = (c1, c2) -> data.get(c1).getOrDefault(c2, 0);

        var jobs = data.keySet().stream()
                .flatMap(c1 -> data.keySet().stream()
                        .map(c2 -> List.of(c1, c2)))
                .map(l -> ((Executable) () -> assertEquals(distance.applyAsInt(l.get(0), l.get(1)), distance.applyAsInt(l.get(1), l.get(0)))))
                .toList();

        assertAll(jobs);
    }
}
