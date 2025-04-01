using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HfPark
{
    public class Guest
    {
        private List<Gift> prizes;
        private string name;

        public Guest(string n)
        {
            name = n;
            prizes = new List<Gift>();
        }

        public string getName() { return name; }
        public void Wins(Gift g)
        {
            if (g.targetShot == null) throw new ArgumentNullException();
            if (!g.targetShot.GetGifts().Contains(g)) throw new ArgumentException();
            g.targetShot.GetGifts().Remove(g);
            prizes.Add(g);
        }
        public int Result(TargetShot t)
        {
            int s = 0;
            foreach(var e in prizes)
            {
                if (e.targetShot == t) s += e.Value();
            }
            return s;
        }
    }
}
