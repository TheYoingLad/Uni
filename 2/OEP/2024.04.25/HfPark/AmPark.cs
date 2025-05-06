using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HfPark
{
    public class AmPark
    {
        private List<TargetShot> targetShots;
        private List<Guest> guests;

        public AmPark(List<TargetShot> t)
        {
            if (t.Count < 2) throw new ArgumentException();
            targetShots = t;
            guests = new List<Guest>();
        }

        public string Best(TargetShot t)
        {
            if (guests.Count == 0) throw new ArgumentException();
            int maxind = 0;
            for (int i = 1; i < guests.Count; i++) if (guests[i].Result(t) > guests[maxind].Result(t)) maxind = i;
            return guests[maxind].getName();

        }
        public void Receives(Guest g)
        {
            if (guests.Contains(g)) throw new ArgumentException();
            guests.Add(g);
        }
    }
}
