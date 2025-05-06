using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace elfOrkCsata
{
    public abstract class Harcos
    {
        private string nev;
        protected int hp;
        protected int kincs;
        protected Tulajdonsag tul;

        public Harcos(string n, int h, int k)
        {
            nev = n;
            hp = h;
            kincs = k;
            tul = Tulajdonsag.Instance();
        }

        public string getNev() { return nev; }
        public int getHp() { return hp; }
        public int getKincs() { return kincs; }

        public int Meghal()
        {
            int k = kincs;
            kincs = 0;
            hp = 0;
            return k;
        }
        public bool Tamad(Harcos h)
        {
            if (this == h) throw new ArgumentException("Saját magát nem támadhatja meg!");
            int e = Ero();
            (bool halott, int k) = h.Vedekez(e);
            if (halott)
            {
                kincs += k;
                return true;
            }
            return false;
        }
        public (bool halott, int k) Vedekez(int t)
        {
            if(t < 0) throw new ArgumentOutOfRangeException("Nemnegatív erővel lehet támadni");
            int p = Pajzs();
            if (t >= p + hp) return (true, Meghal());
            if (t > p) hp -= t - p;
            return (false, 0);
        }
        public abstract int Ero();
        public abstract int Pajzs();
        public abstract int MaxHp();
        public abstract string Tipus();
    }
}
