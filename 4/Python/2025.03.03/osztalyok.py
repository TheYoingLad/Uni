class BankAccount:
    def __init__(self, b : int = 0) -> None: #ctor
        self.balance = b

    def deposit(self, x : int = 0) -> None:
        self.balance += x

    def withdraw(self, x : int = 0) -> None:
        self.balance -= x

    def return_balance(self) -> int: #self-en belül van a osztály minden adattagja, kell mint paraméter ha az osztályon akarunk dolgozni
        return self.balance

    @staticmethod
    def print_balance(account):
        return account.return_balance()

b = BankAccount(10)
print(b.return_balance())
print(BankAccount.print_balance(b))