package test;

import city.City;
import org.junit.jupiter.api.Test;
import posta.Posta;
import posta.Postafiok;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class PostaTest {

    @Test
    void test() {
        Postafiok f1 = new Postafiok();
        Postafiok f2 = new Postafiok();
        Postafiok f3 = new Postafiok();
        Postafiok f4 = new Postafiok();

        Posta p1 = new Posta(City.BUDAPEST, f1, f2);
        Posta p2 = new Posta(City.LONDON, f3, f4);

        p1.celZipToPosta = Map.of(
                1234, p2
        );

        p2.celZipToPosta = Map.of(
                4321, p2
        );
    }

    @Test
    void felad() {
    }

    @Test
    void main() {
    }
}