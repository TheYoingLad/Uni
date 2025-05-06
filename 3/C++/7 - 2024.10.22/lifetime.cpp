#include <iostream>

struct Lifetime{
    std::string s;
    Lifetime () {std::cout << "@" << this << " DefCtor\n";}
    Lifetime (const std::string& s): s(s) {std::cout << s << " @" << this << " DefCtor\n";}
    
    Lifetime (const Lifetime& other): s(other.s) { std::cout << s << " @" << this << " CopyCtor\n"; } //alapból ha nem írjuk meg, akkor a compiler generál egyet, ami minden adattagot átmásol
    
    //             V jobb érték referencia, jobban köt a jobb értékekre => ez fog lefutni ha jobb ref-et kap
    Lifetime (Lifetime&& other): s(std::move(other.s)) {std::cout << s << " @" << this << " MoveCtor\n";} 
    //                                  ^ "kilopja" a stringet

    //Lifetime (Lifetime&& other) = delete; // nincs move konstruktor

    Lifetime& operator=(const Lifetime& other) {
        if (this == &other) return *this; //megéri-e ez a check, ha töredék százalék esetben történne meg? => Profilozás, inputtól függ
        s = other.s;
        std::cout << s << " @" << this << " CopyAssign\n";
        return *this;
    }
    
    Lifetime& operator=(Lifetime&& other) {
        if (this == &other) return *this; //megéri-e ez a check, ha töredék százalék esetben történne meg? => Profilozás, inputtól függ
        s = std::move(other.s);
        std::cout << s << " @" << this << " MoveAssign\n";
        return *this;
    }

    //Lifetime& operator=(Lifetime&&) = default; //compiler által generált implementáció

    ~Lifetime() {std::cout << s << " @" << this << " Dtor\n";}
};

//Lifetime global_l; //init sorrend nem definiált; statikus élettartam; DefCtor, Dtor

//Lifetime global_l("global");

int main(){
    //Lifetime l; //autómatikus élettartam; DefCtor, Dtor

    //Lifetime *l = new Lifetime(); //dinamikus élettartam; DefCtor

    //Lifetime l("local");
    //Lifetime l2 = l; // }
    //Lifetime l3(l);  // } =>Copy konstruktor
    //Lifetime l4{l};  // }

    //Lifetime l("local");
    //Lifetime l2 = l;
    //l2 = l; // Copy Asign
    
    //Lifetime l = Lifetime("temp"); //optimalizáció => le se fut a temp konstruktora és az átmásolás, hanem egyből az l konstruálódik, még ha mellékhatásokkal is járna a copy

    Lifetime l("local");
    Lifetime l2 = std::move(l);
}