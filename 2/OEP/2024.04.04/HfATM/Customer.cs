using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HfATM
{
    public class Customer
    {
        private string pin;
        private int withdraw;
        private List<Account> accounts;

        public Customer(string p, int w)
        {
            pin = p;
            withdraw = w;
            accounts = new List<Account>();
        }

        public void Withdrawal(ATM a) { a.Process(this); }
        public Card ProvidesCard() { return accounts[0].cards[0]; }
        public string ProvidesPIN() { return pin; }
        public int RequestsMoney() { return withdraw; }
        public void AddAccount(Account a) { accounts.Add(a); }
    }
}
