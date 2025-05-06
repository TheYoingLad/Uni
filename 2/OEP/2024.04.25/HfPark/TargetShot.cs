using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HfPark
{
    public class TargetShot
    {
        private string site;
        private List<Gift> gifts;

        public TargetShot(string s)
        {
            site = s;
            gifts = new List<Gift>();
        }

        public List<Gift> GetGifts() { return gifts; }
        public void Shows(Gift g)
        {
            if (g.targetShot != null) throw new ArgumentNullException();
            if (gifts.Contains(g)) throw new ArgumentException();
            g.targetShot = this;
            gifts.Add(g);
        }
    }
}
