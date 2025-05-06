using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HfStarship
{
    public class Planet
    {
        public string name;
        private List<Starship> ships;

        public Planet(string n)
        {
            name = n;
            ships = new List<Starship>();
        }
        public void Defends(Starship s)
        {
            if (ships.Contains(s)) throw new ArgumentException();
            ships.Add(s);
        }
        public void Leaves(Starship s)
        {
            if (!ships.Contains(s)) throw new ArgumentException();
            ships.Remove(s);
        }
        public int ShipCount() { return ships.Count; }
        public int ShieldSum()
        {
            return ships.Sum(e => e.GetShield());
        }
        public (bool, double, Starship?) MaxFireP()
        {
            bool van = false;
            double max = -1;
            Starship? s = null;
            foreach(var e in ships)
            {
                if(!van)
                {
                    van = true;
                    max = e.FireP();
                    s = e;
                } else if(e.FireP() > max)
                {
                    max = e.FireP();
                    s = e;
                }
            }
            if (!van) return (false, 0, null);
            return (true,  max, s);
        }
    }
}
