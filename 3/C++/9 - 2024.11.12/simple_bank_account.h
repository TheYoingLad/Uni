#pragma once

#include <iostream>
#include <string>

class SimpleBankAccount {
  std::string m_account_number;
  double m_balance;

public:
  SimpleBankAccount(const std::string &account_number,
                    double initial_balance = 0.0)
      : m_account_number(account_number), m_balance(initial_balance) {}

  double getBalance() const { return m_balance; }
  std::string getAccountNumber() const { return m_account_number; }

  virtual void deposit(double amount) {
    if (amount < 0) {
      std::cerr << "Cannot deposit negative amount" << std::endl;
      return;
    }

    m_balance += amount;
  }

  void nonVirtualDeposit(double amount) {
    if (amount < 0) {
      std::cerr << "Cannot deposit negative amount" << std::endl;
      return;
    }
    m_balance += amount;
  }

  virtual std::string getAccountType() const { return "Simple"; }

  void withdraw(double amount) {
    if (amount < 0) {
      std::cerr << "Cannot withdraw negative amount" << std::endl;
      return;
    }
    if (amount > m_balance) {
      std::cerr << "Insufficient funds to withdraw $" << amount << std::endl;
      return;
    }

    m_balance -= amount;
  }

  virtual ~SimpleBankAccount() = default;
};
