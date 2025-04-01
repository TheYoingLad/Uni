using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HfATM
{
    public class Bank
    {
        private List<Account> accounts;

        public Bank() { accounts = new List<Account>(); }

        public void OpenAccount(string cNum, Customer c)
        {
            Account a = new Account(cNum);
            c.AddAccount(a);
            accounts.Add(a);
        }
        public void ProvidesCard(string cNum)
        {
            (bool l, Account a) = FindAccount(cNum);
            if (l) a.cards.Add(new Card(cNum, "1234"));
        }
        public int GetBalance(string cNum)
        {
            (bool l, Account a) = FindAccount(cNum);
            if (l) return a.GetBalance();
            return -1;
        }
        public void Transaction(string cNum, int n)
        {
            (bool l, Account a) = FindAccount(cNum);
            if (l) a.Change(n);
        }
        public bool CheckAccount(string cNum)
        {
            (bool l, Account a) = FindAccount(cNum);
            return l;
        }
        private (bool, Account) FindAccount(string cNum)
        {
            int i = 0;
            while (i < accounts.Count && accounts[i].accNum != cNum) i++;
            if (i < accounts.Count) return (true, accounts[i]);
            return (false, null);
        }
    }
}
