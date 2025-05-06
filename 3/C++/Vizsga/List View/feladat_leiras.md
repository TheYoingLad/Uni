# 2024.01.02 C++ zh feladat:

## Rövid leírás:

A feladat két lita "nézet" megvalósítása lényegében ezek annyit tudnak hogy a nézeten keresztül a lista mindegyik eleme O(1) időn belül elérhető. A lista az std::list 2 irányú láncolt lista
invariánsok: Ők nem adnak új elemet, vagy törölnek elemet miután neked odaadták

## array_view<T,Cont>

- template osztály, paraméterei:
    - T - lista elemek típusa
    - Cont - Conténer típus alapértelmezetten legyen az std::vector
- fix mérete van
- megvalósítja a .at(idx) és [idx] műveleteket
- az iterátorokat tárolja, így érd el az elemeket O(1)-ben

## vector_view<T, Cont>

- Örökölje meg az array_view-ot (ez nem volt egyértelmű elvárás, azt mondták lehet enélkül is, de így ajánlott, ami igazi feltétel volt az az hogy at(idx) [idx] legyen)
- push_back(T) legyen! (azaz nem lesz fix a size, a beszúrt elemet be kell szúrni az eredeti listába!!!)
- valósítsa meg a sort()-ot és lehessen paraméterként megadni felhasználó által definiált rendezést (a sort nem a listát rendezi, hanem a Cont-ot)
