package exam.sinbad.sky;

import exam.sinbad.Adventure;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class ValuableAdventure extends Adventure {
    private Map<Bird, Integer> birdDiamonds;


    public ValuableAdventure(String diamondFilename, Bird... b) throws IOException {
        super(b);
        birdDiamonds = new HashMap<>();
        try (BufferedReader br = new BufferedReader(new FileReader(diamondFilename))) {
            String line = br.readLine();
            while (line != null) {
                String[] data = line.split(" ");
                Ankaa bird = new Ankaa(data[0]);
                birdDiamonds.put(bird, Integer.parseInt(data[1]));
                line = br.readLine();
            }
        }
    }


    @Override
    protected void doEscape(Bird b){
        super.doEscape(b);
        if(birdDiamonds.containsKey(b)){
            storeDiamonds(birdDiamonds.get(b));
            birdDiamonds.remove(b);
        }
    }
}
