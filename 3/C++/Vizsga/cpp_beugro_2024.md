# C++ 2024 beugró kérdések
## ha a filet "feldolgozva" nézed nem fogod látni hogy melyik válaszok vannak helyesnek megjelölve míg, ha forráskódként olvasod ezeket is láthatod!
<!--A dokumentumban megjelölt "helyes" válaszok csak szerintem helyesek és a vizsgán 3 hibás válaszom volt!!!-->

1. Milyen sorrendben futnak le az alábbi eszközök a C/C++ programok fordításakor?

- Nyelvi fordító, linker, interpreter.
- Preprocesszor, nyelvi fordító, linker. <!--Correct-->
- Preprocesszor, interpreter, nyelvi fordító.
- Interpreter, nyelvi fordító, linker.

2. Mi a kimenete egy szabványosan leforduló C/C++ kód preprocesszálásának?

- Bytekód
- C/C++ forráskód <!--Correct-->
- Tárgykód
- Assembly kód

3. Az alábbi f függvény kapcsán melyik függvényhívás fordul le?
```C++
void f( const int *a, const int& b )
{
}

// A válasz
 const int s = 2;
 const int t = 1;

 f( &s, &t );


// B válasz
 int s = 2;
 const int t = 1;

 f( 0, s );


// C válasz
 int s = 2;
 int t = 1;

 f( const &s, const &t );


// D válasz
 const int s = 2;
 int t = 1;

 f( &s, &t );
```
<!--B is correct-->

4. Melyik állítás igaz az alábbiak közül?

- A C++ sablonok példányosításához a new operátort használjuk.
- A C++ sablonokból nem generálódik kód a példányosítása előtt. <!--Correct-->
- A C++ sablonok példányosítását futási időben végezzük.
- A C++ sablonok példányosításához a Word körlevél funkcióját használjuk.

5. Melyik kódrészlet helyes az alábbi osztálydefiníció kapcsán?
```C++
class X
{
public:
  X( int i ) { }
};

// A válasz
 void f()
 {
   X s;
 }

// B válasz
 void f()
 {
   X s = new X( 3 );
 }

// C válasz
 void f()
 {
   X s = null;
 }

// D válasz
 void f()
 {
   X s( 3 );
 }
```
<!--D is Correct-->


6. Melyik deklarációra illeszkedik a csillaggal jelölt sorban meghívott művelet?
```C++
class Foo
{
...
};

Foo f;
Foo g;
f  = g; // (*)
```


- Foo::operator=(Foo& lhs, Foo& rhs);
- Foo& Foo::operator=(const Foo& rhs); <!--Szerintem-->
- Foo::Foo(Foo rhs);
- Foo::Foo(const Foo& rhs);


7. Az alábbiak közül melyik szabványos STL konténer?
<!--A cpp ref-en csak a multisetet találtam meg-->
- std::multiset\<T> <!--Correct-->
- std::rb_tree\<T>
- std::tree_set\<T>
- std::binary_tree\<T>



8. Melyik állítás igaz az alábbiak közül?

- Egy absztrakt osztálynak több konstruktora is lehet C++-ban.
- Egy absztrakt osztálynak minden metódusa virtuális C++-ban. <!--Correct-->
- Egy absztrakt osztálynak több destruktora is lehet C++-ban.
- Az abstract kulcsszóval jelöljük az osztály absztraktságát C++-ban.


9. Az alábbi függvényhívás melyik függvénydeklarációra nem illeszkedik?
```C++
f( "abcdef" );
```


- void f( std::string );
- void f( const char* );
- void f( std::string& ); <!--Correct, az éppen létre jövő változókat nem veheted át referenciaként, mert nem változtatható az eredeti-->
- void f( const std::string& );

10. Melyik művelettel szabadítjuk fel a new-val allokált tárterületet C++-ban?

- free
- clear
- delete <!--Correct-->
- Nullpointerre állítjuk azt a pointert, amelyik rámutat a dinamikusan allokált tárterületre.


11. Az alábbiak közül melyik C++, de nem C kulcsszó?
- long
- extern
- void
- operator <!--Correct-->


12. Melyik C++ kulcsszó az alábbiak közül?
- virtual <!--Correct?-->
- vector
- 0xAL
- std::vector


13. Mit ír ki az alábbi kódrészlet?
```C++
#include <iostream>

int main()
{
  int x = 5;
  std::cout << x;
  if ( x > 3U || ( x = 8 ) )
  {
    std::cout << x;
  }
}
```

- 38
- 58
- 5
- 55 <!--Correct, U jelentése unsigned-->


14. Mit ír ki az  alábbi kódrészlet?
```C++
#include <iostream>

struct X
{

  X( int i = 3 ) { std::cout << i; }
  ~X() { std::cout << 2; }

};

int main()
{
  X a( 5 );
  X* p = new X( 4 );
}
```

- 34
- 3422
- 54
- 542 <!--Correct, a p változónak nem jön el az élettartalma vége, nem hívódik meg a desktruktor-->


15. Melyik állítás igaz az alábbiak közül?
- A heap-en lévő adatok automatikusan felszabadulnak, a stack-en lévőek nem szabadulnak automatikusan.
- A stack-en lévő változók értékét fordítási időben ismerni kell, a heap-en lévő adatok értékét nem kell ismerni fordítási időben.
- A stack-en lévő változók méretét futási időben ismerni kell, a heap-en lévő adatok méretét nem kell ismerni futási időben.
- A stack-en lévő változók méretét fordítási időben ismerni kell, a heap-en lévő adatok méretét nem kell ismerni fordítási időben. <!--Correct-->
