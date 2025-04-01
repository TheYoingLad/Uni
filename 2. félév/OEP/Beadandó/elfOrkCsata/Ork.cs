using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace elfOrkCsata
{
    public abstract class Ork : Harcos
    {
        public Ork(string n, int h, int k): base(n, h, k) { }
    }

    public class Verengzo : Ork
    {
        public Verengzo(string n, int k) : base(n, 100, k) { }

        public override int Ero() { return tul.Tul(this).e; }
        public override int Pajzs() { return tul.Tul(this).p; }
        public override int MaxHp() { return tul.Tul(this).h; }
        public override string Tipus() { return "vérengző"; }
    }

    public class Ravasz : Ork
    {
        public Ravasz(string n, int k) : base(n, 90, k) { }

        public override int Ero() { return tul.Tul(this).e; }
        public override int Pajzs() { return tul.Tul(this).p; }
        public override int MaxHp() { return tul.Tul(this).h; }
        public override string Tipus() { return "ravasz"; }
    }

    public class Ovatos : Ork
    {
        public Ovatos(string n, int k) : base(n, 80, k) { }

        public override int Ero() { return tul.Tul(this).e; }
        public override int Pajzs() { return tul.Tul(this).p; }
        public override int MaxHp() { return tul.Tul(this).h; }
        public override string Tipus() { return "óvatos"; }
    }
}
