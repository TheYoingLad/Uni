#pragma once

#include "simple_bank_account.h"

#include <string>
#include <vector>

//struct == class, csak alapértelmezetten a classon belüli dolgok private-ek, a structon belül meg public-ok

class A {};

class B : A {}; // == class B : private A, mert B kontextusán belül vagyunk


/*

öröklődés \ A | private | protected | public    |
------------------------------------------------|
private       |  -----  | private   | private   |
protected     |  -----  | protected | protected |
public        |  -----  | protected | public    |

*/


struct Transactionable {
  virtual void deposit(double) = 0; //tisztán virtuális metódus => nem lehet példányosítani
  virtual void withdraw(double) = 0;

};


class TransactionalBankAccount : public SimpleBankAccount, public Transactionable {
  std::vector<std::string> transaction_history;

public:
  TransactionalBankAccount(const std::string &account_number,
                           double initial_balance = 0.0)
      : SimpleBankAccount(account_number, initial_balance) {}

  std::vector<std::string> getTransactionHistory() const {
    return transaction_history;
  }

  void deposit(double amount) override {
    SimpleBankAccount::deposit(amount);
    transaction_history.push_back("Deposited $" + std::to_string(amount));
  }

  void nonVirtualDeposit(double amount) {
    SimpleBankAccount::nonVirtualDeposit(amount);
    transaction_history.push_back("Deposited $" + std::to_string(amount));
  }

  std::string getAccountType() const override { return "Transactional"; }

  void withdraw(double amount) {
    SimpleBankAccount::withdraw(amount);
    transaction_history.push_back("Withdrew $" + std::to_string(amount));
  }
};
