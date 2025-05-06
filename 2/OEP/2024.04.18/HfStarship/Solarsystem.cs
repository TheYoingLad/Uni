using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HfStarship
{
    public class Solarsystem
    {
        public List<Planet> planets;

        public Solarsystem() { planets = new List<Planet>(); }

        public (bool, Starship?) MaxFireP()
        {
            bool van = false;
            double max = -1;
            Starship? s = null;
            foreach (var e in planets) if (e.MaxFireP().Item1)
                {
                    if (!van)
                    {
                        van = true;
                        max = e.MaxFireP().Item2;
                        s = e.MaxFireP().Item3;
                    }
                    else if (e.MaxFireP().Item2 > max)
                    {
                        max = e.MaxFireP().Item2;
                        s = e.MaxFireP().Item3;
                    }
                }
            if (!van) return (false, null);
            return (true, s);
        }
        public List<Planet> Defenseless()
        {
            List<Planet> undefended = new List<Planet>();
            foreach (var e in planets) if (e.ShipCount() == 0) undefended.Add(e);
            return undefended;
        }
    }
}
