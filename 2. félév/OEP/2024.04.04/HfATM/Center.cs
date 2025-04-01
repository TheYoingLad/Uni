using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HfATM
{
    public class Center
    {
        private List<Bank> banks;

        public Center(List<Bank> b) { banks = b; }

        public int GetBalance(string cNum)
        {
            (bool l, Bank b) = FindBank(cNum);
            if (l) return b.GetBalance(cNum);
            return -1;
        }
        public void Transaction(string cNum, int n)
        {
            (bool l, Bank b) = FindBank(cNum);
            if (l) b.Transaction(cNum, n);
        }
        private (bool, Bank) FindBank(string cNum)
        {
            int i = 0;
            while (i < banks.Count && !banks[i].CheckAccount(cNum)) i++;
            if (i < banks.Count) return (true, banks[i]);
            return (false, null);
        }
    }
}
