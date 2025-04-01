using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HfATM
{
    public class Card
    {
        public string cNum;
        private string pin;

        public Card(string c, string p)
        {
            cNum = c;
            pin = p;
        }

        public bool CheckPIN(string p) { return pin == p; }
        public void SetPIN(string p) { pin = p; }
    }
}
