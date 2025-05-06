package posta;

import city.City;
import week.WeekDay;

import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class Posta {
    public City city;
    public Map<Integer, Posta> celZipToPosta;
    public Postafiok[] postafiokok;

    public Posta(City city, Map<Integer, Posta> celZipToPosta, Postafiok... fiokok) {
        this.celZipToPosta = celZipToPosta;
        this.city = city;
        postafiokok = fiokok;
    }

    public Posta(City city, Postafiok... fiokok) {
        this.celZipToPosta = new HashMap<>();
        this.city = city;
        postafiokok = fiokok;
    }

    public void tovabbit(Level level){
        int cel = level.zipCode;
        if(cel == city.getZipCode()){
            felad();
        } else {

        }
    }

    public void felad(){

    }

    public static void main(String[] args) {
        Level level;
    }
}
