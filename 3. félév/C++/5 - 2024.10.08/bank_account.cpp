#include "bank_account.h"
#include "person.h"

std::string BankAccount::owner_name() { return owner.name; }

void BankAccount::operator+(double d){
    balance += d;
} 

std::ostream& operator<<(std::ostream& stream, const BankAccount& acc){
    stream << "Account balance: " << acc.balance << '\n';
    return stream;
}