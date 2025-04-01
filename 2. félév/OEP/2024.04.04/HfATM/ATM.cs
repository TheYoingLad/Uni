using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HfATM
{
    public class ATM
    {
        public string site;
        private Center center;

        public ATM(string s, Center c)
        {
            site = s;
            center = c;
        }

        public void Process(Customer c)
        {
            Card card = c.ProvidesCard();
            if (card.CheckPIN(c.ProvidesPIN()))
            {
                int a = c.RequestsMoney();
                if (center.GetBalance(card.cNum) >= a) center.Transaction(card.cNum, -a);
            }
        }
    }
}
