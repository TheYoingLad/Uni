using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HfStarship
{
    public class Starship
    {
        private string name;
        protected int shield;
        protected int armor;
        protected int guardian;
        private Planet? planet;

        public Starship(string n, int s, int a, int g)
        {
            name = n;
            shield = s;
            armor = a;
            guardian = g;
            planet = null;
        }
        public int GetShield() { return shield; }
        public void StaysAtPlanet(Planet p)
        {
            if (planet != null) planet.Leaves(this);
            planet = p;
            p.Defends(this);
        }
        public void LeavesPlanet()
        {
            if (planet == null) throw new ArgumentException();
            planet.Leaves(this);
            planet = null;
        }
        public virtual double FireP() { return 0; }
    }
    
    public class Wallbreaker : Starship
    {
        public Wallbreaker(string n, int s, int a, int g) : base(n, s, a, g) { }

        public override double FireP() { return armor / 2; }
    }

    public class Landingship : Starship
    {
        public Landingship(string n, int s, int a, int g) : base(n, s, a, g) { }

        public override double FireP() { return guardian; }
    }

    public class Lasership : Starship
    {
        public Lasership(string n, int s, int a, int g) : base(n, s, a, g) { }

        public override double FireP() { return shield; }
    }
}
