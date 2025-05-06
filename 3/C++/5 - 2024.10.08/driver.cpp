#include "bank_account.h"
#include "person.h"

#include <iostream>

int main(){
    Person p("Alice", 35.2, 123123);
    BankAccount acc(1000, p);

    acc + 1000;

    std::cout << "Account owner: " << acc.owner_name() << '\n' << acc;
}