#include <string>
//#include "person.h" jobb mint include "person.cpp"
//                    rosszabb mint a forward deklaráció (class Person;)
#include <ostream>

class Person;

class BankAccount{
    double balance;
    Person& owner;
    
public:
    /*
    BankAccount(double b, Person& p){
        balance = b;
        owner = p; //probéma: owner nincs incializálva!
                   //referencia membert nem lehet konstruktor törzsben inicializálni
    }
    */

    BankAccount(double b, Person& p): balance(b), owner(p){}
    std::string owner_name();
    void operator+(double d);
    friend std::ostream& operator<<(std::ostream& stream, const BankAccount& acc);
};