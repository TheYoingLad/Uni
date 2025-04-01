# C++

## 2. gyakorlat

### Láthatóság és élettartam


- Automatikus valtozok: elettartam, az ertekuk memoriaszemet alapertelmezetten
    ```c++
    #include <iostream>

    int main() {
        int x = 6; // automatikus valtozo
        {
            /* uj scope */
            int x = 8; // learnyekolja/elfedi a kulso x-et: nem lathato, de elettartama tart
            std::cout << x << '\n';
        }
    }
    ```

- Globalis valtozok: a program az o inicializaciojukkal kezdodik; a program futasanak vegeig elnek

    ```c++
    #include <iostream>

    int i = 9;

    namespace detail {
        int k;
    }

    int main() {
        detail::k = 7; // k a detail nameespace resze ~-~ i ekkor a globalis nevter resze
        for (int i = 0; i < 1; ++i) {
            std::cout << ::i << '\n'; /* ez itt a globalis i */
        }
    }
    ```
    - gyenge nyomonkovethetoseg (tobb thread modositja egyszerre - UB)
    - inicializacio sorrendje a linkelesi sorrendtol fugg - rossz nyomonkovethetoseg

- Ertektipusok
  - balertek: lehet a cimuket kepezni - ertekadas bal oldalan all tipikusan
  - *jobbertek*: nem lehet cimet kepezni hozza (pl. ilyen a szamliteral -- `&7` nem valid)
  - C++11-ben: fastruktura - `lvalue`, `xvalue` (temporalis), `prvalue` (pure rvalue - sehogyan sem cimkepezheto)
    - `glvalue`: `lvalue` vagy `xvalue`
    - `rvalue`: `xvalue` vagy `prvalue`
    - vannak jobbertekek, amiknek a cimet szeretnenk kepezni (rvalue ref)

- Memoriamodelel:
  - amikor megkerem az oprendszert: kapok egy stack-et az oprendszertol - a stack kezdetben ures
  - valtozo letrehozasa: megjelenik a stacken
  - dinamikus memoriakezeles - heap => az mashogy mukodik

### Pointerek

- cel: tudjunk indirekt valtoztatni valamilyen valtozot
```c++
#include <iostream>

int main() {
    int k = 6;
    int *ptr = &k;
}
```

- C++ const vs Java final: erteket nem lehet egyszer sem adni, csak inicializalni - utana nem valtozhat meg sehogy

- `NULL` (`(void*)0`) helyett *C++11*-től `nullptr` -- jobb

  ```c++
  void f (int*) {}
  void f (int) {}

  int main() { f(NULL); } /* ambiguous: illeszkedik mindketto f definiciora */

  /* nullptr - tipusa: std::nullptr_t */
  ```

- nem standard C++ a VLA!
  ```c++
  int main() {
    int N;
    std::cin >> N;
    int arr[N]; /* ilyet ne! */
  }
  ```

- tomb tudja magarol a meretet, de megis tiszta adat (nincs `size` adattagja - a tipusrendszer tudja!)
    - fontos: a `sizeof` operator, nem fuggveny!

- tomb atvetele parameterkent (a meretet is tudjuk!)
  ```c++
  void g(int (*p)[5]) {
    std::cout << sizeof(p) << '\n';
  }

  // ez csak 5 elemu parameterket tud atvenni parameterul!
  ```

- parameter (ami a deklaracioban van) vs argumentum (amivel meghivjuk a ffuggvenyt)

- `std::endl` -- ne hasznaljuk!
    - az IO muveletek nagyon lassuak
    - sporolni akarunk a szamukkal: egy bufferbe toltunk bele eloszor; csak akkor irunk ki, ha betelik a buffer
    - => a valosagban kevesebb syscall, mint `std::cin::operator >>`
    - de az `std::endl` a sortores kiirasan kivul egy `flush` (WC lehuzas :lol:) muveletet is csinal

- a `login.cpp` mintat kerjuk el a kovi improg gyakorlatra!!!

- *rovid az elet, hosszu a sir, es keves van hatra a gyakorlatbol*

## 3. gyakorlat

### Referenciák
- `swap` függvény implementáció példa (egyik: pointert swapoltunk a pointer által mutatott érték helyett)
- konstans pointer: `int *const`; konstansra mutató pointer: `const int *`
  - `int *a = nullptr, b = 2` -- itt a `b` nem pointer -- ezért konvenció: a csillag jelet a változó _nevéhez_ írjuk
-  **Két dolog, amit mindig inicializálni kell: konstansok és referenciák**

### Típusok

- `unsigned`-nak negatív érték értékadása: jól definiált viselkedés (és a maximumról való overflow is az)
  - `int`-eknél UB

- szabvány: `sizeof(short) <= sizeof(int) <= sizeof(long) <= sizeof(long long)`
  - "a 30 éves mosógép firmware-jébe nem fér bele, hogy bazinagy int-eket hozzunk létre"
- `sizeof(unsigned X) == sizeof(X)`
- `sizeof(char) == 1` (mindig)
- hány bit egy bájt? (szabvány szerint legalább 8)

- itt van `bool` típus is; `true` és `false` kulcsszavak
  - `sizeof(bool) >= sizeof(char)` (a karakter az egység; de nem feltétlenül egyenlő)

```c++
struct A {
  int i;
  long c;
  int k;
};

struct B {
  int i;
  int k;
  long c;
};


sizeof(A) > sizeof(B)
| i | k |  c  |

nem éri meg a procinak, hogy a c változó szó-határon legyen
| i |   |  c  | k |  | /* padding beszúrása *s/
```

- minden pointere ugyanakkora: `sizeof(X*) == sizeof(Y*)`

- kérdés: érték vagy referencia szerint érdemes átvenni egy változót?
  - ökölszabály: ha az érték kisebb, mint a pointer, érdemes érték szerint átvennis

  ```c++
  bool value (int l, int r) {
    if (l < r)
      return true;
    l += 1;
    return l > r;
  }

  bool ref(int &l, int &r) {
    if (l < r)
      return true;
    l += 1;
    return l > r;
  }

  int main() {
    int a = 5, b = 5;
    std::cout << value(a, b) << ' ' << ref(a, a) << '\n';
                                     // aliasing!
  }
  ```

  - két pointert összehasonlítani (amik nem ugyanazon tömb elemeire mutatnak) nem definiált viselkedés
    ```c++
    bool ptr(int *l, int *r) {
      if (l < r) // ez itt rossz, nem azt csinalja, amit szeretnenk
        return true;
      *l += 1;
      return l > r;
    }
    ```

- hányféleképpen lehet egy pointer null? `nullptr`, `NULL` és `0`
  - overloadingnál számít
  -
  ```c++
  void f(int);
  void f(int*);
  void f(std::nullptr_t);

  int main() {
    f(0); /* int - oké, mert int*-hoz már kellene 1 konverzió */
    f(NULL); /* NULL = (void*)0; meghíváshoz void* -> int* konverzió, vagy void* -> int konverzió -- AMBIGUOUS */
    f(nullptr); /* C++11, ezt érdemes használni */
  }
  ```
  - szabály: mindig azt hívja meg, amihez a legkevesebb konverzióra van szükség

  - assemblyben nem lehet, hogy két ugyanolyan nevű függvényünk legyen -> a függvény nevébe a paramétereket is belekódoljuk (_name mangling_)

### Operátortúlterhelés

```c++
struct Complex {
  int re, im;
}

Complex plus(Complex lhs, Complex rhs) {
  Complex ret { lhs.re + rhs.re, lhs.im + rhs.im };
  return ret;
}

Complex operator+(Complex lhs, Complex rhs) {
  Complex ret { lhs.re + rhs.re, lhs.im + rhs.im };
  return ret;
}

int main() {
  Complex c1 { 1, 2 }; /* C-style/struct initialization */
  Complex c2 { 1, 2 };

  Complex ret = plus(c1, c2);
  // Complex ret = operator+(c1, c2);
  Complex ret = c1 + c2;
  std::cout << ret.re << ' ' << ret.im << '\n';
}
```

- istenem, annyira imádom ezt a nyelvet, de mennyire egy rakás szar...
- "aki azzal akar foglalkozni, hogy milyen problémát akar megoldani, inkább kerülje"
- nem csak a `+`, hanem a `<<` operátort is túl lehet terhelni
```c++
void operator<<(std::ostream &out, Complex c) {
  out << '(' << c.re << ", " << c. im << ")";
}

std::ostream &operator<<(std::ostream &out, Complex c) {
  out << '(' << c.re << ", " << c. im << ")";
  return out;
}

std::cout << ret; // mukodik
std::cout << ret << '\n'; // nem jó, nincs operator<<(void, char)
/*
  ((std::cout << ret) << '\n') // de pl. az értékadás jobb-asszociatív (!!!)

  ezért vissza kell adni a stream-et, hogy tudjuk láncolni
*/
```

- "a sírós emoji operátort nagy valószínűséggel nem lehet túlterhelni"

### iostream

- írjunk olyat, ami eltol magánhangzókat

```c++
// encrypt.cpp
#include <iostream>

int main() {
  char c;
  std::cout << "Please provide your input:\n";
  std::cin >> c;

  if (std::cin.fail())
    return -1;
  
  char from[5] = { 'a', 'e', 'i', 'o', 'u'};
  char to[5]   = { 'e', 'i', 'o', 'u', 'a'};

  for (int i = 0; i < sizeof(from)/sizeof(from[0]); ++i) {
    if (c == from[i])
      c = to[i]; // kovetkezo iteracioban megint felul fogja irni!! - ki kell lepni a ciklusbol!
                 // break-et nyugodtan lehet itt hasznalni
  }

  std::cout << "Your encoded input: " << c << '\n';
}
```

- "a literálok mind konstansok: a 6-nak nem adhatom mégiscsak értékül, hogy 7"

- `"Hello"` típusa: `char const[6]` (a nul-terminátor is benne van)

```c++
char *c = "Hello"; // ISO C++ forbids converting a string constant to 'char*'
                   // de csak warning, a C backward compatibility miatt van igy

c[2] = 'k'; // segmentation fault

sizeof(std::string("Hello")); // ez nem a karakterek szamat adja meg - sizeof()!
```

- itt `32` byte a `std::string` - miért?
  implementáció: Small String Optimization - ha kicsi stringeim vannak, sokkal hatekonyabb ha a heap-en nem allokalunk

  nálam például:
  ```
  ❯ cat test.cpp
  #include <string>
  #include <iostream>

  int main() {
    std::cout << sizeof(std::string) << '\n';
    std::cout << std::string().capacity() << '\n';
  }
  ❯ ./a.out
  24
  22
  ```

"időközben a macska már lepisilte a hard drive-ot, már nem lehet kibreak-elni"
TODO: Ctrl+D tanítása ehhez kapcsolódóan!
```c++
// encrypt.cpp
#include <iostream>

char encode(char c) {
  char from[5] = { 'a', 'e', 'i', 'o', 'u'};
  char to[5]   = { 'e', 'i', 'o', 'u', 'a'};

  for (int i = 0; i < sizeof(from)/sizeof(from[0]); ++i) {
    if (c == from[i])
      return to[i];
  }
  return c;
}
int main() {
  std::cout << "Please provide your input:\n";

  std::string s;
  while (std::cin >> s) {
    for (int i = 0; i < s.size(); ++i)
      std::cout << encode(s[i]);
  }
  /*
  Please provide your input:
  edeit
  Your encoded input:
  idiot
  */
```

# 4. gyakorlat

## iostream
- `std::noskipws` -> ne hagyja ki a whitespace karaktereket (ez pontosan mi?)
- `std::istream &std::getline(std::istream &, std::string &)` -> egy teljes sort olvas be a paraméterként kapott stringbe
  - van még olyan overload is, ami egy _delimitert_ is kér
```
/* input.txt */
3
alma meggy
korte
barack
```
```c++
/* Szelethus: progalapon az egesz evfolyama szivott ezzel */
int main() {
  int n:
  std::string tmp;
  std::cin >> n;
  for (int i = 0; i < n; ++i) {
    std::getline(std::cin, tmp);

    /* árulkodó jel: az utolsó sor nem jelenik meg, az elején extra üres sor van */
    std::cout << tmp;
  }
}
```

  - `std::getline` a szám után az 1. sor végéig lévő whitespace karaktert olvassa csak be először
    - `std::cin` a leading whitespace karaktereket veszi ki a streamből
    - `std::getline` a sor végit
    - megoldás: `std::cin.ignore()` a getline hívása előt -> kiveszi a streamből a következő (newline) karaktert
  
  - `stringstream` is van

## `static` kulcsszó

- különböző kontextusokban másféle jelentése van

- fordítási-egység-statikus változók: nagyon hasonlóak a globális változókhoz -- csak a láthatóságuk a fordítási egységre korlátozódik
  => több TU-ban ugyanolyan néven definiálva -> két külön változó lesz
  "őket kevésbé utáljuk, mint a globálisakat. A scope-ja nagyon le van limitálva"
- függvényre nézve statikus változó
  - az első függvényhívás előtt inicializálódik, és a program futása alatt csak egyszer
  - ha többször hívjuk a függvényt, nem fog újra inicializálódni mindig
~~(Husi azt mondta, hogy ez is 0-inicializálódik. Nem!)~~ DE: a feltétel: _static storage duration_ (globális változók, `static` _is_) => ekkor zero-inicializálás történik
  ```c++
  void f() {
    static int i = 0;
    std::cout << ++i << '\n';
  }

  int main() {
    f(); // 1
    f(); // 2
    f(); // 3
  }
  ```
- osztály statikus adattagja
  ```c++
  struct A {
    static int count; // változó DEKLARÁCIÓ
  };

  // csak így magában: "undefined reference to A::count"
  // definiálnunk is kell valahol -- ha headerbe tesszük és több helyen include-oljuk: kellene, hogy csak 1 példány legyen belőle
  // az osztályon kívül kell definiálni:
  //
  // int A::count = 0;
  //

  int main() {
    A a1, a2;
    a1.count = 6;
    std::cout << a2.count << '\n'; // 6
  }
  ```

## Dinamikus memóriakezelés
- memóriamodell: 3 nagy egység: stack, heap, globális/statikus

- ne felejtsük el free-elni
  - az operációs rendszer kilépés után úgyis felszabadítja
  - de ne szivárogtassunk memóriát: mindig szabadítsuk fel, amit lefoglaltunk
  - _nem a pointert töröljük ki, hanem felszabadítja a memóriaterületet, amire az mutat. A pointert békén hagyja_
- C: `malloc`, `free`; C++: `new`, `delete` (NEM keverjük őket!)
- `malloc` nagyon fapados: csak kapunk néhány byte-ot, semmi inicializáció nem történik
   - `new`-val viszont meghívódik a konstruktor pl. `int *p = new int(5);`

- fejelemes láncolt lista megírása (fejelem nélkülivel nehezebb kifejezni, ha üres a lista -- bárhogy megoldjuk, az effektive a fejelem egy implementációja lenne)

```c++
struct Node {
  int data;
  Node *next;
};

void display(Node *head) {
  Node *ptr = head;
  while (ptr) {
    std::cout << ptr->data << ' ';
    ptr = ptr->next;
  }
}

int main() {
  Node *head = nullptr;
  head = new Node { 5, nullptr };
  head->next = new Node { 6, nullptr };
  head->next->next = new Node { 7, nullptr };

  display(head);

  /* ezt fordítsuk `-fsanitize=address`-szel: memóriaszivárgások! */

  /*
  delete head;
  delete head->next;
  delete head->next->next;
  // heap use after free 
  */
}
```

```c++
void push_back(Node *head, int data) {
  if (head == nullptr) {
    // ez még nem oldja meg a helyzetet: a main-en belül lévő head pointer null marad
    head = new Node { data, nullptr };
    return;
  }

  Node *ptr = head;
  while (ptr->next) {
    ptr = ptr->next;
  }
  ptr->next = new Node { data, nullptr };
}

int main() {
  Node *head = nullptr;
  push_back(head, 5);
  push_back(head, 6);
  push_back(head, 7);
  
  delete head->next->next; // null deref!
}
```

megoldás: egy osztályban implementáljuk a láncolt listát, ne pucér head pointereket használjunk
```c++
struct List {
  Node *head; // !!!

  /*
  List() : head(nullptr) {}
  */
};

void push_back(List &l, int data) {
  if (l.head == nullptr) {
    // ez még nem oldja meg a helyzetet: a main-en belül lévő head pointer null marad
    l.head = new Node { data, nullptr };
    return;
  }

  Node *ptr = l.head;
  while (ptr->next) {
    ptr = ptr->next;
  }
  ptr->next = new Node { data, nullptr };
}

void free(List &l) {
  Node *ptr = l.head;
  while (ptr) {
    Node *nextptr = ptr->next;
    delete ptr;
    ptr = nextPtr;
  }
}

int main() {
  List l; // ez nem inicializálja a head pointert, ha nincs konstruktor
  /* l.head = nullptr; */
  push_back(l, 5);
  push_back(l, 6);

  display(l);
}
```

- konstruktor írása
```c++
struct A {
  int a, b;

  // a konstruktorok konstruálnak (a destruktor viszont nem destruál)
  A(int i, int j) {
    // itt már létre is jött az objektum - először nem inicializált a és b, amikor a ctor törzse el kezd futni
    a = i;
    b = j;
  }

  // inicializációs lista - mindig az adattagok definíciós sorrendjét követi (!!!)
  A(int i, int j) : a(i), b(j) [

  ]
};

int main() {
  A a1 (5, 6);
  // A a2; // nem fog működni, mert nincs default konstruktor - ha egyetlen konstruktort is megírtunk, mást nem hoz létre alapértelmezetten
}
```

- metódusoknak van egy _implicit paramétere_: `this`

# 5. gyakorlat

```c++
struct A {
  A() {}
};

int main() {
  A a(); // nem a default konstruktor hívódik meg, hanem egy a nevű függvényt deklarálunk!!
}
```

- konstruktor konstruál, de a destruktor nem destruál (hasonlóan move nem move-ol, stb.)

- erőforrások felszabadítása - nem akarjuk minden early return-nél külön meghívni a memória felszabadítást
  - megoldás: **RAII** (Resource Acquisition is Initialization): a konstruktorban allokálunk erőforrásokat, a destruktorban szabadítjuk fel őket
    - ez determinisztikus, nem úgy mint a garbage collection: amint kimegy a scope-ból
  - ez a C++ népszerűvé válásának egyik legfőbb oka: nem kell félni, hogy nem lesz valami felszabadítva
    az objektumok automatikusan feltakarítanak maguk után
  - pl. az `std::cout` is így írja ki a bufferben lévő szöveget (ha flush/endl nem volt belecsövezve)

- példa: RAII láncolt lista - egyiknek értékül adjuk a másikat
  - a függvény végén crashel a program, miért?
  - a stack-en van 2 list objektumunk, `l1` és `l2`
  - `list l2 = l1` -> mindkét objektum ugyanazokra az allokált node-okra mutat
  - l1 felszabadításakor l2 már felszabadította a node-okat!
  - megoldás: hogyan kell a listát helyesen másolni? (ne ugyanazokra mutasson, hanem két _ekvivalens_ lista jöjjön létre)
  `List l2 = l` != `l2 = l1` (konstruktor hívás - új változó másik mintájára, a másik _értékadás_) EZ KÉT KÜLÖNBÖZŐ MŰVELET! \[beugrón szinte biztos, hogy lesz\]

- értékadó operátorból magunkra mutató referenciát szokás visszaadni (hogy láncolható legyen)
  - lista: előbb szabadítsuk fel a meglévő node-jainkat, utána pusholjuk az új node-okat
  - NEM tudunk a copy ctorra hivatkozni benne: minden objektum az élettartama során csak egyszer jöhet létre

- önmagunknak való értékadás - -hiba lehet belőle
  ```c++
  if (this == &other)
      return this;
  ```

- **Rule of 3**: ha destruktor, értékadó op. vagy másoló konstruktor hármasból valamelyiket meg kell írni, akor a másik 2-t is!
  - (Rule of 5 C++11-től: move assignment és move constructor)
  - haladó C++-on lesz: MSc-s tárgy, kérvénnyel felvehető

  - reguláris típus: lásd jegyzet
    - minden más speciális tagfüggvényt tud a compiler generálni, kivéve egyenlőségvizsgálat operátort

- láthatóság: `private` és `public`
  - az öröklődésnél is érdekes lesz: `protected`, maga az inheritance is kap láthatóságot

```c++
void push_back(int data) {
  Node **ptr = &head;
  while (*ptr) {
    ptr = &(*ptr)->next;
  }
  *ptr = new Node { data, nullptr };
}
```

# 6. gyakorlat: Iterátorok

- GT OEP: a konténer osztályon (pl. vektor) belül volt implementálva a felsorolás logikája
  - itt: nem az osztályhoz kell hozzányúlni (pl. extra index/current ptr adattagot hozzáadni)
  - a pointer általánosítása, külön adatszerkezet
  - vektor iterátor típusa: `std::vector<T>::iterator` (`begin()`, `end()` adja vissza, ezek a `vector` tagfüggvényei)

- (forward) iterátor
  - léptetés (`++`)
  - összehasonlítás (`!=`, `==`)
  - adott elem lekérdezése (`*`)

- tömbnél, vektornál utolsó utáni elem: size() index; listánál: null pointer

```c++
class Iterator {
  Node *ptr;

public:
    Iterator(Node *p) : ptr(p) {}

    Iterator operator++() {
      ptr = ptr->next;
      return *this; // lancolas miatt igy erdemes
    }

    bool operator==(Iterator other) const {
      return ptr == other.ptr;
    }

    bool operator!=(Iterator other) const {
      return !(*this == other); // implementalas az operator== altal; visszavezetes az egyenlosegre
    }

    int operator*() {
      return ptr->data;
    }
};

struct List {
  /* ... */

  Iterator begin() {
    return Iterator(head);
  }

  Iterator end() {
    return Iterator(nullptr);
  }
};

void display() {
  for (Node *ptr = head; ptr != nullptr; ptr = ptr->next) {
    std::cout << ptr->data << ' ';
  }
}
```

- tagfüggvények szétválasztása deklarációra és definícióra
   ```c++
   struct Iterator {
    Iterator operator++();
   };

   // mintha egy Iterator névtéren belül lenne egy függvényünk
   Iterator Iterator::operator++() {
    // ...
   }
   ```

- "na most kapcsolja be mindenki a biztonsági övet, kemény lesz!"

```c++

const List l2(l);
l2.display(); // passing 'const List' as 'this' argument discards qualifiers
              // (qualifier: `const`/`volatile`)
```

- "gondolkodjunk ezen egy kicsiket: hogy a francban lehet ez egy helyes kod"

- `const_iterator` osztály: a visszaadott _pointerek_ is konstansok (hogy const listára mutató iterátoron keresztül ne lehessen elemet módosítani!)
  - ezt nem jegyzeteltem le

- postfix `++`: miért nem érdemes használni ezt?
  - hogy írunk meg posztfix `++` operátort?

  ```c++
  Iterator &++operator() {} // ilyen nem letezik!
  ```

  megoldás: üres int paraméter, hogy meg tudjuk a kettőt különböztetni
  ```c++
  /* nem referenciát ad vissza: másolatot! */
  Iterator operator++(int) {
    Iterator tmp = *this;
    ++(*this);
    return tmp; // visszatérés a megnövelés előti értékkel!
  }
  ```

- operátoroknál: implicit `this`-en való konverzió nem működik -- azaz csak a jobb oldali operandusban

```c++
struct Complex {
  Complex(int);

  Complex operator+(Complex) const;
};

int main() {
  Complex a = 2;

  Complex b = a + 2; // mukodik!
  Complex c = 2 + a; // 2 nem tud konvertalodni Complex-re
}
```

## Több fájl

- `linked_list.h`, `linked_list.cpp`, `main.cpp`
  - `linked_list.h`: deklarációk   - HEADER GUARD! (vizsgán ez a minimumszint! - nem lehet pragma once)
  - `linked_list.cpp`: definíciók
  - `main.cpp`: használat
- ha függvény definíciót teszünk a headerbe (pl. out-of-line operátor _definíciók_): multiple definition linker hiba
  - de inline fv def (azaz a `class` blokkon belül - akkor még fordul)
  - ODR: csak a strong symbol-okra érvényes, az inline-ok weak-ek

- osztályon belül minden tagfüggvény inline-nak számít
  - ha több helyen különböző definíció van - a linker nem definiált, hogy melyiket választja

```c++
inline void List::display() const {
  // ...
}
```

# 7. gyakorlat

## Több fájlra szétbontás (folyt.)

- kód olvashatósága, kódmegértés - a fejlesztők idejének fele-háromnegyede kód olvasásáról szól
  - "ha nehezen olvasható ganét írunk, pontenciálisan a mi pénzünket ereszti le a lefolyón"
  - mindent feláldozni azért, hogy rövidebb legyen a kód, semmi értelme nincsen. Általában inkább az olvashatóság kárára megy

- konverziós konstruktor
- hogy lehessen konstans iterátort létrehozni nem konstans iterátor segítségével? (másik irányban nem is értelmes)

  ```c++
  class ConstIterator {
    Node *ptr;
  
  public:
    ConstIterator(Iterator it) : ptr(it.ptr) {} // error: az iterator ptr member-e privát
                                                // megoldás: legyen a konsans a friend-je a nem konstansnak (ez nem szimmetrikus reláció!)
  }
  ```

  - másik megoldás: konverziós operátor (a kettő közül mindig csak 1 kell)
  - az iterátort tanítjuk meg arra, hogy konstans iterátorrá átalakítsa magát
    - ez a kevésbé elterjedt verzió (ezt nagyon nem szokták szeretni?!)
  ```c++
  class Iterator {
    // ...
  public:
    operator ConstIterator() {
      return ConstIterator(ptr);
    }
  }
  ```
  
  - ezeket a globális névtérben definiáltuk -> semmi másik ilyen nevű osztályt nem lehet létrehozni
    - névterek segítségével oldjuk meg, hogy ne szemeteljük tele a globális névteret

  ```c++
  namespace detail {
    class Iterator { /* ... */ };
  } // end of namespace detail

  namespace detail {
    class ConstIterator { /* ... */ };
  } // end of namespace detail
  ```

  - de most a használat helyén kelleni fog kiírni a namespace-t! megoldás:
  ```c++
  class List {
    // C++98:
    typedef detail::Iterator Iterator;
  
    // C++11 (modern C++-ban - fv. pointerek használata esetén sokkal olvashatóbb):
    using ConstIterator = detail::ConstIterator;
  }
  ```

## Generikus programozás

- bevezető példa: `swap` függvény más típusokon is működjön!
```c++
void swap(int &a, int &b);

int main() {
  float f1 = 1, f2 = 2;

  swap(a, b); // ez nem fordul!
}
```

- overload-olással meg lehet csinálni, hogy copy-paste-tel minden típusra csinálunk egy másolatot -- de ne!

```c++
template <class T>
void swap(T &a, T &b) {
  T tmp = a;
  a = b;
  b = tmp;
}

int main() {
  int i1 = 1, i2 = 2;
  swap<int>(i1, i2);

  swap(i1, i2); // template argument dedukció - nem kell explicit módon kiírni!
                // tipusoknal nincsen - dedukciós guide-ok (CTAD) c++17-től
}
```

- _példányosítás_
- lehet típusmegkötést megadni hozzá? - később nézzük!

```c++
// nem csak típusok lehetnek template paraméterek - non-type template parameter

template<int N>
void printN() {
  std::cout << N << '\n';
}
```

- a fordító példányosítja ezt - azaz nem lehet futás során kiderülő értéket megadunk paraméterként
  - a mangled name-be bekerül a NTTP-k teljes értéke
  - nem lehet: karakkterlánc literál és float érték (pontatlanság)

- template-ek lustán példányosulnak
```c++
template<class T>
struct A {
  T data;

  void foo() {
    data.helloworld(); // ez nem okoz fordítási hibát A<int>-nél, amíg a foo()-t meg nem hívjuk
  }
}
```

- tömb paraméter
  ```c++
  void f1(int *t, int size) {}
  void f2(int (&t)[5]) {} // ezt még nem mondtam improgon!

  template<class T, int Size>
  int getSize(T (&t)[Size]) {
    return Size;
  }
  ```

- metaprogramozás

```c++
template<int N>
struct factorial {
  static const int value = N * factorial<N - 1>::value;
};

template<>
struct factorial<0> {
  static const int value = 1;
      // ^---- ez ide kell: a nem-konstans statikus adattagoknak out-of-line definíció kell (hisz a módosításnak ugyanoda kell mennie mindig) !!!!
};

int main() {
  std::cout << factorial<5>::value << '\n';
}
```

- template-ekkel írtunk egy programot - ciklus és elágazás
- ezt mind fordítási időben számolja ki

- template is lehet template parameter
```c++
template <
  template<class>
  class Container
>
struct Matrix {
  Container<int> cont;
}
```

- dependent scope: nem tudjuk megmondani, hogy az adott kifejezés egy statikus adattag vagy típus alias lesz-e
```c++
template <class T>
struct A {
  using X = int;
}

void foo() {
  A<int>::X k;
}

template<class T>
void foo() {
  A<T>::X k; // need 'typename' before 'A<T>::X' because 'A<T>' is a dependent scope
}

template<class T>
void foo() {
  typename A<T>::X k;
}
```

  - miért nem tudja a fordító megoldani ezt? Specializációban úgyis megcsinálhatjuk, hogy típus helyett statikus változó legyen itt

  - (C++20)

- hamarosan ismét +/-

# 8. gyakorlat

- példa:
```c++
// linked_list.h

std::ostream &operator<<(std::ostream &out, const List &l);
```

- ha betesszük a headerbe az `iostream` include-ot; tranzitíven a projekt összes többi fájljába is belekerül
  - de az iostream nagyon nagy (több tízezer sor) -> a fordítási idő nagyon megnő
  - megoldás: forward deklaráció: `<iosfwd>` header (amíg csak referenciaként hivatkozunk rá, elég lesz)

- osztály template: a linked list osztályunk ne csak int-eket tudjon tárolni!
  -  minden template lesz: a node template, de az iterátor is ilyet tárol -> neki is template-nek kell lennie

```c++
template <class T>
class Iterator {
  Node<T> *ptr;
  // ...
}
```

- inkább referenciával térjen vissza az iterátor - másolás elkerülése nagy típusok esetén is

```c++
  // v fordítási hiba: dependent scope - C++20 előtt nem tudja megmondani a fordító, hogy ConstIterator az static member vagy type alias
  // a typename kulcsszó kell
for (List<T>::ConstIterator it = l.begin(); it != l.end(); ++it) {
  std::cout << *it << ' ';
}
```

- szinte biztos, hogy beugróban lesz róla szó

## Standard Template Library

- szekvenciális konténerek: `vector`, `list`, `deque`, `string` (kinda)
- asszociatív konténerek: `set`, `map`, `multiset`, `multimap`
- konténer adapterek: `stack`, `queue`, `priority_queue`


- `vector` = dinamikus méretű tömb
  - beszúrás műveletigénye: amortizált konstans; ha a kapacitás és a méret megegyezik (nincs benne szabad hely), akkor minden elemet át kell másolni új allokálása miatt
  - _iterátor invalidáció_: ha a vector átméreteződik, az iterátorok érvénytelenné válnak
  - dangling pointer, dangling iterátor: olyan memóriaterületre mutat, ami az átméretezés miatt felszabadításra került

  - vector feltöltése inicializációs listával csak c++11 óta
  - `at` kivételt dob, ha túlindexelünk
  - `operator[]` UB

  - C++11 range based for loop
  ```c++
  for (int i : v)
    std::cout << i << ' ';

  // erre fordul le:
  for (std::vector<int>::iterator it = v.begin(); it != v.end(); ++it) {
    int i = *it;
    std::cout << i << ' ';
  }
  ```

  - vector `insert`: ha a közepére szúrunk be, az összes utána lévő elemet át kell mozgatni

# 9. gyakorlat

- konténereknekben allokátor template paraméter: pl. a globális new-t lecserélhetjük olyan allokátorra, ami egy előre lefoglalt tömbbe helyezi el (memory pool)
- `std::set`: halmaz - minden elemet csak egyszer tárol; az elemeket rendezetten tárolja
  - `std::multiset`: az ekvivalens elemeket is többször tárolja
  - ábrázolása: valamilyen bináris keresőfa (red-black, AVL) [a szabvány csak a szükséges komplexitást írja le]

  ```c++
  std::set<int> s;
  s.insert(5);

  std::set<int>::itetrator it = s.begin();
  *s = 5; // fordítási hiba: a set iterátora konstans referenciát ad -- felülírással a BST invariánsát meg lehetne sérteni
  ```

  ```c++
  struct Point {
    int x, y;
  }

  void foo() {
    std::set<Point> s;
    s.insert(Point(1, 2)); // (ocsmány) fordítási hiba: "no match for operator< (operand types are 'const Point' and 'const Point')"
  }
  ```

  - template paraméter a rendezést implementáló **funktor** ((itt van a fogalom bevezetve)) -- alapértelmezetten: `std::less<T>`


  ```c++
  using vec_iter = std::vector<int>::iterator;

  // találja meg az első adott tulajdonságú elemet! -> predikátum függvény pointer
  //                                      itt a * nem kötelező
  // https://cdecl.org
  vec_iter find(std::vector<int> &v, bool (*op)(int)) {
    for (vec_iter it = v.begin(); it != v.end(); ++it) {
      if (op(*it))
        return it;
    }

    return v.end();
  }

  // kérdés: hogy tudnánk kifejezni, hogy a második ilyet találja meg!
  // -> statikus változó, számolgassuk, hogy hányat találtunk meg!
  // kérdés: mi történik, ha több szálon futtatjuk?
  // "itt egy race condition van: fejreáll a processzor, teherbe esik a macska, összeomlik az egész világ"

  bool is_even(int i) { return i % 2 == 0; }

  // megoldás: funktor -> van függvényhívó operátora (lokális state-et tárol)
  struct IsSecondEven {
    int count = 0; // C++11 az in-class inicializáció

    bool operator()(int i) {
      if (i % 2 == 0) {
        ++count;

        if (count == 2)
          return true;
      }
      return false;
    }
  };

  template<class UnaryPred>
  vec_find it(std::vector<int> &v, UnaryPred op) {
    for (vec_iter it = v.begin(); it != v.end(); ++it) {
      if (op(*it))
        return it;
    }

    return v.end();
  }

  int main() {
    std::vector<int> v = { 1, 2, 3, 4, 5 };

    // vec_ier i = find(v, is_even);
    vec_itetr i = find(v, IsSecondEven {});

    if (it == v.end())
      return 1;

    std::cout << *it << '\n';
  }
  ```

  - **funktor**: olyan típus, aminek felül van definiálva a gömbölyű zárójel operátora
    - **unáris predikátum**: egy paramétert vár, `bool`-t ad vissza
    - **bináris predikátum**: két paramétert vár, `bool`-t ad vissza

 - csúnya példa
 ```c++
 struct PointLess {
    bool operator()(const Point &lhs, const Point &rhs) {
      return lhs.x < rhs.x;
    }
 };

  std::set<Point, PointLess> s;
  s.insert(Point(1, 2));
  s.insert(Point(2, 3));
  s.insert(Point(1, 3));

  // mi történik? csak 2 eleme lesz a halmaznak
  // egyenlőség meghatározása a funktor alapján: !(a < b) és !(b < a) => a == b ("<" itt a funktor operátora)
 ```

# 10. gyakorlat

- `std::map`
  - set-hez hasonló belső reprezentáció (kiegyensúlyozott bináris fa)
  - kulcs-érték párokat tárol (`std::pair` -- `first`: kulcs; `second`: érték)
  - érték megváltozhat, csak kulcs alapján lehet lekérni
  - hiányzó kulcs esetén default konstruktorral beszúrás történik [`operator[]` mellékhatásos] (ez _jól definiált viselkedés_)
  - kerüljük ennek a használatát; inkább `find`-ot használjunk vagy `count`-ot, illetve `insert(std::pair<const K, V>(k, v))`-t
    - vagy még egyszeűbben: `insert(make_pair(k, v))` - nem kell kiírni a template paramétereket - template argument deduction
  - rendezés a kulcs szerint => érték szerint nem lehet kerensi benne; viszont az érték így módosítható
    - adott értékű elem keresésére lehetőség: iterálás (mondjuk `std::find_if`)

- iterátorok osztályozása
  - input
  - forward
  - bidirectional
  - random access

- input vs forward: az input iterátoron csak egyszer lehet végigmenni
- `std::advance` - iterátor előre mozgatása - generikusan
  - random access iterátorok esetén konstans idő, `operator+=` által
  - egyéb esetekben `++` operátor használata, lineáris idő

  - eldöntés: `iterator_category` typedef (pl. `std::bidirectional_iterator_tag`)
    - üres osztály, a különböző típusok közti leszármazással

    - _dispatcher function_

```c++
template<class RandAccIter>
void advance(RandAccIter &it, int n, std::random_access_iterator_tag) {
  /* fast */
  it += n;
}

template<class BidirIter>
void advance(BidirIter &it, int n, std::bidirectional_iterator_tag) {
  /* slow */
  if (n > 0) {
    for (int i = 0; i < n; ++i)
      ++it;
  } else {
    for (int i = 0; i < -n; ++i)
      --it;
  }
}

template<class Iter>
void advance(Iter &it, int n) {
  advance(it, n, typename Iter::iterator_category());
}
```

- crtp, sfinae

- `std::iterator` template (nézd meg!) CRTP leszármazással
  - C++17-től deprecated -- helyette manuálisan typedef

## STL algoritmusok

- `std::find_if` unáris predikátum alapján keres
  - `std::find_if(v.begin(), v.end(), IsEven {})` -- lambdát nem lehe használni még!

- `std::sort` -- az ekvivalens elemek sorrendjét nem feltéteeln tartja meg
  - példa: modulo 3 maradék megegyezése funktor
  - megoldás elemek relatív sorrendjének megőrzésére: `std::stable_sort`

- `std::find`
  - `set`, `map` esetén a tagfüggvény használata gyorsabb

# 11. gyakorlat

- `std::partition`: egy unáris predikátum alapján szétválasztja a konténer elemeit
  - ld. példa: nincs meghatározva az ekvivalens elemek relatív sorrendje
  - ha kell: `std::stable_partition`
  - sort esetén is hasonlóan van stable verzió

- hogy ne sortoljunk?
  - `std::vector` -> átmásolás `std::set`-be -> visszamásolás `std::vector`-ba

- `std::remove`/`std::remove_if`
  - egy konténerből bizonyos tulajdonságú elemeket _kitöröl_
  - a konténer elejére begyűjti azokat az elemeket, amik nem teljesítik a predikátumot (azokat, amik maradnak)
  - visszatérési érték: iterátor arra a részre, ahol a kitörlendő elemek kezdődnek
    - (a törlendő részt nem-definiáltan hagyja)

  ```c++
  std::vector<int> v = { 1, 2, 3, 4, 5, 6, 7, 8, 9 };

  auto it = std::remove_if(v.begin(), v.end(), Even());

  v.erase(it, v.end());
  ```

- objektumelvű programozás
  - enkapszuláció (C-ben is: struct, több adat egy kompozit adatstruktúrában)
  - adatelrejtés  (pl. struct definíciója csak az implementációs fájlba kerül)
  - kód újrafelhasználás

  - _aki objektumorientáltan programozik C-ben, az eléggé jellemfejlesztő_

  - C-ben nincs öröklés

```c++
class Shape {
  int x, y;

public:
  Shape(int x, int y) : x(x), y(y) {}

  // konstruktordelegáció
  Shape(int x) : Shape(x, 0) {}

  int area() const;

  void print() const {
    std::cout << "Shape\n";
  }
};

// öröklődés
class Circle : public Shape {
  int r;

public:
  Circle(int x, int y, int r) : Shape(x, y), r(r) {}

  int area() const {
    return 3.14 * r * r;
  }

  void print() const {
    std::cout << "Circle\n";
  }
};
```

- öröklődés esetén milyen sorrendben futnak le a konstruktorok és destruktorok?
  - konstruktor: az ősöktől a legleszármazottabb osztályokig
  - destruktor: fordított sorrendben (logika: a leszármazottnak "része" az ős -> előbb kell azt felszabadítani)

- slicing
  - C++-ban előfordul, de más nyelvekben nem
  - mert itt az osztályok nem referenciatípusok, hanem pl. a stacken is létre tudunk hozni ilyeneket
  - ha Shape-pel szeretnénk Circle objektumra hivatkozni -> poineren vagy referencián keresztül

  - változóról infó:
    név, cím, érték, statikus típus, dinamikus típus

  - `virtual` kulccszó: explicit eltérés az _olcsó megoldástól_: tagfüggvényt ne a statikus, hanem a dinamikus típus szerint hívja meg

  - **polimorfikus típus**: olyan típus, amelynek legalább egy virtuális tagfüggvénye van
  - implementáció: `vtable`: az összes virtuális tagfüggvény címe globális konstans táblában
    a példányokban pedig egy pointer (vptr) erre a táblára.
  - a polimorfizmus nem ingyenes, sőt nem is olcsó

  - `override` kulcsszó: ha ősosztályban virtuálisként van definiálva, implicit az lesz a leszármazottban is.
  - Az `override` kulcsszó (C++11) fordítási hibát dob, ha nem írunk felül meglévő virtuális függvényt (pl. const-ság, egyéb banális hiba)


- tisztán virtuális függvény: nincs implementáció
  - az ilyeneket tartalmazó osztályok absztraktnak számítanak - nem példányosíthatóak
  - hogy lehet még megoldani, hogy ne legyen egy típus példányosítható - privát konstruktor vagy destruktor

- public, protected és private öröklés
  - _aki lát protected öröklést, az kérem, írjon azonnal nekem e-mailt_
  - public: minden adattag és tagfüggvény megőrzi az access specifier-ét
    private: minden adattag és tagfüggvény private lesz
    protected: public -> protected

  - private örölődés haszna: ne lehessen polimorfikusan használni, az ősosztályra átcast-olni
    pl. ha STL kontérnerből származunk le, amiknek nincs virtuális destruktora

- `dynamic_cast`: futási időben típus lekérése/base-to-derived cast

- következő gyakorlat: többszörös öröklődés, virtuális öröklődés, gyémántöröklődés

# 12. gyakorlat

- többszörös öröklődés
  - sok nyelvben nem engedélyezett
  - Java-ban: csak egy ősosztály lehet, csak interfészekből implementálhat többet (vagy osztály, aminek nincs adattagja?)

  - mikor lehet ilyennek értelme?
  
  - diamond inheritance
  ```c++
  struct Shape {
    int x, y;
  };

  struct Rombus : public Shape {};

  struct Trapezoid : public Shape {};

  struct Square : public Rombus, public Trapezoid {
    void print() {
      std::cout << "x, y: " << x << ", " << y << '\n';
      // ERROR: reference to x is ambiguous
      // ERROR: reference to y is ambiguous
    }
  };
  ```

  - hogy lehet ezt kijavítani? Melyik irányból megörökölt adattagra szeretnénk hivatkozni? (minden egyes felhasználási helyen!)
  ```c++
  std::cout << "x, y: " << Rombus::x << ", " << Rombus::y << '\n';
  ```

  - de ez nem helyes: a négyzetnek nyilván nem két koordináta-párja van!

  - megoldás: virtuális öröklés -> a fordító figyel arra, hogy az adattagok csak egyszer legyenek jelen
  ```c++
  struct Rombus : virtual public Shape {};
  struct Trapezoid : virtual public Shape {};
  ```
  - de ennek is overhead-je van (mi történik, ha polimorfikusan szeretném haszálni?)
  - ne használjuk!

  - a standarban gyémánt öröklődésre példa: basic_istream és basic_ostream is is örököl a basic_ios-ból
    - belőlük pedig a basic_iostream
  

- upcasting: implicit módon működik
- downcasting: `dynamic_cast` kell hozzá
  ```c++
  void foo(Shape *s) {
    Circle *c = dynamic_cast<Circle *>(s);

    // ha referencia: exception-t dob, ha nem sikerül a cast-olás

    if (c)
      std::cout << "It's a circle\n";
  }
  ```

```c++
enum MyEnum {
  a,
  b,
  c,
};

int main() {
  int i = MyEnum::a; // működik, implicit konverzió enum értékek és integerek között
}
```
- C++11-től `enum class`: implicit konverzió letiltása (de a működés ugyanaz: számok 0-tól felsorolva, explicit konverzió használata)
  ebben az esetben muszáj: `static_cast`
- `static_cast`:
  - `int i = static_cast<int>(MyEnumClass::a);`
  - C-s gömbölyű zárójeles cast-olás helyett használjuk! -> ez beleerőszakolja úgy, ahogy van és kész
  
- _a fordítótól fogunk egy taslit kapni -- szerintem azt fogunk kapni_

- ha van legacy függvényünk, ami konstansként tekint egy pointert, csak még régi C-ben készült, amikor nem létezett a const kulcsszó:
  - `const_cast`
  - minden más esetben ne használjuk - UB lehet
  ```c++
  struct A {};
  void foo(A *a);

  int main() {
    const A a;
    foo(const_cast<A *>(&a));
  }
  ```

- `reinterpret_cast`
  ```c++
  int *ptr = i;
  intptr_t iptr = reinterpret_cast<intptr_t>(ptr);
  ```

  - majdnem ugyanolyan durva, mint a C-style cast
  - két teljesen unrelated pointer típus közötti cast-olás
  - még példa, ahol van értelme: `std::optional` implementációja: buffer átcast-olása a tárolt típusra

- void pointer esetén a `static_cast` a preferált [nézz utána]


- hátramaradt dolgok (aztán készen vagyunk!)
  - explicit konstruktor
  ```c++
  // a korábban megírt verzióban

  List<int>::iterator it = nullptr; // a belső reprezentáció egy pointer, van pointert váró konstruktora, ezért ez értelmes (utolsó utáni pointer)

  // de ne szabadjon így megcsinálni! van ilyen konstruktora: -> ez egy Node<T>* -> Iterator _konverziós konstruktor_!
  Iterator(Node<T> *ptr) : ptr(ptr) {}
  ```
    - ne működjön konverziós konstruktorként, explicit meg kelljen hívni a konstruktort!
    ```c++
    explicit Iterator(Node<T> *ptr) : ptr(ptr) {}

    List<int>::iterator it = nullptr; // fordítási hiba
    List<int>::iterator it(nullptr);
    ```

    - persze van, amikor _jó_ az implicit konverzió: pl. iterator -> const_iterator között

  - new operátor tömbre
  ```c++
  int main() {
    int *ptr = new int[10];

    delete[] ptr; // sima `delete` UB -> ezzel utasítjuk a nyelvet, hogy a méret metaadatot is vegye figyelembe
  }
  ```
  - viszont ugyanúgy nem tudjuk megkérdezni tőle (kódban), hogy hány elemre van hely lefoglalva
    - miért kell: hány elemre futtasson le destruktort a delete közben -- ha triviális destruktora van a típusnak, kihagy(hat)ja
    - https://itanium-cxx-abi.github.io/cxx-abi/abi.html#array-cookies
  
  - fontos: new/delete és malloc/free nem kombinálható

  - lifetime extension (nézz utána!)
    ```c++
    struct A {};
    void f(A const &a) {}
    void bar(int const &b) {}

    int main() {
      f(A{}); // a temporális érték élettartama a függvényhívás végéig tart
      bar(5); // ez is működik itt
    }
    ```
    - _név nélküli temporális változó_

- következő óra: minta beugró, vizsga (olyan, ami nincs bent Kristóf jegyzetében)

https://people.inf.elte.hu/szelethus/LaTeX/cpp/cpp_book/cpp_book.pdf