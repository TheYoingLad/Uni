#include "simple_bank_account.h"
#include "transactional_bank_account.h"

#include <iomanip>
#include <iostream>
#include <limits>
#include <memory>
#include <vector>

class BankTUI {
private:
  std::vector<std::unique_ptr<SimpleBankAccount>> accounts;

  void clearScreen() {
#ifdef _WIN32
    system("cls");
#else
    system("clear");
#endif
  }

  void waitForEnter() {
    std::cout << "\nPress Enter to continue...";
    std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
  }

  void addAccount() {
    std::string accountNumber, ownerName;
    double initialBalance;
    int accountType;

    std::cout << "\nEnter account number: ";
    std::cin >> accountNumber;
    std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');

    std::cout << "Enter owner name: ";
    std::getline(std::cin, ownerName);

    std::cout << "Enter initial balance: $";
    std::cin >> initialBalance;

    std::cout << "Select account type:\n";
    std::cout << "1. Simple Bank Account\n";
    std::cout << "2. Transactional Bank Account\n";
    std::cout << "Choice: ";
    std::cin >> accountType;

    std::string fullAccountNumber = accountNumber + " (" + ownerName + ")";

    if (accountType == 1) {
      accounts.push_back(std::make_unique<SimpleBankAccount>(fullAccountNumber,
                                                             initialBalance));
    } else {
      accounts.push_back(std::make_unique<TransactionalBankAccount>(
          fullAccountNumber, initialBalance));
    }
  }

  void listAccounts() {
    std::cout << "\nCurrent Accounts:\n";
    std::cout << std::setfill('-') << std::setw(60) << "-" << std::endl;
    std::cout << std::setfill(' ');
    std::cout << std::left << std::setw(5) << "ID" << std::setw(30)
              << "Account Number (Owner)" << std::setw(15) << "Type"
              << std::setw(10) << "Balance" << std::endl;
    std::cout << std::setfill('-') << std::setw(60) << "-" << std::endl;
    std::cout << std::setfill(' ');

    for (size_t i = 0; i < accounts.size(); ++i) {
      std::cout << std::left << std::setw(5) << i << std::setw(30)
                << accounts[i]->getAccountNumber() << std::setw(15)
                << accounts[i]->getAccountType() << "$" << std::fixed
                << std::setprecision(2) << accounts[i]->getBalance()
                << std::endl;
    }
  }

  void performTransaction() {
    if (accounts.empty()) {
      std::cout << "\nNo accounts available.\n";
      return;
    }

    listAccounts();

    size_t accountId;
    std::cout << "\nEnter account ID: ";
    std::cin >> accountId;

    if (accountId >= accounts.size()) {
      std::cout << "Invalid account ID.\n";
      return;
    }

    int choice;
    std::cout << "\n1. Deposit\n2. Withdraw\nChoice: ";
    std::cin >> choice;

    double amount;
    std::cout << "Enter amount: $";
    std::cin >> amount;

    if (choice == 1) {
      accounts[accountId]->deposit(amount);
    } else if (choice == 2) {
      accounts[accountId]->withdraw(amount);
    }

    // If it's a transactional account, show the history
    if (auto *transAccount = dynamic_cast<TransactionalBankAccount *>(
            accounts[accountId].get())) {
      std::cout << "\nTransaction History:\n";
      for (const auto &transaction : transAccount->getTransactionHistory()) {
        std::cout << "- " << transaction << std::endl;
      }
    }
  }

public:
  void run() {
    while (true) {
      std::cout << "\n=== Bank Account Management System ===\n";
      std::cout << "1. Add Account\n";
      std::cout << "2. List Accounts\n";
      std::cout << "3. Perform Transaction\n";
      std::cout << "4. Exit\n";
      std::cout << "Choice: ";

      int choice;
      std::cin >> choice;

      // Clear screen before processing the next action (except for list accounts)
      if (choice != 2) {
        clearScreen();
      }

      switch (choice) {
      case 1:
        addAccount();
        break;
      case 2:
        listAccounts();
        waitForEnter();
        break;
      case 3:
        performTransaction();
        waitForEnter();
        break;
      case 4:
        return;
      default:
        std::cout << "Invalid choice.\n";
        waitForEnter();
      }
    }
  }
};

int main() {
  BankTUI app;
  app.run();
  return 0;
}
