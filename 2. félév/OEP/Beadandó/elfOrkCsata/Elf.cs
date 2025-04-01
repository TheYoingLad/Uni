using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace elfOrkCsata
{
    public abstract class Elf : Harcos
    {
        protected int elixir;

        public Elf(string n, int h) : base(n, h , 0)
        {
            elixir = 0;
        }

        public int getElixir() { return elixir; }

        public void Atvalt(int n)
        {
            if (n < 0) throw new ArgumentOutOfRangeException("Nemnegatív kincset lehet átváltani");
            if (n > kincs) throw new ArgumentOutOfRangeException("Nincs ennyi kincse!");
            kincs -= n;
            elixir += n;
        }
        public void Iszik(int n)
        {
            if (n < 0) throw new ArgumentOutOfRangeException("Nemnegatív elixirt lehet meginni");
            if (n > elixir) throw new ArgumentOutOfRangeException("Nincs ennyi elixire!");
            int h = MaxHp();
            int c = Math.Min(n, h-hp);
            elixir -= c;
            hp += c;
        }
        public abstract void Gyogyul();
    }

    public class Vakmero : Elf
    {
        public Vakmero(string n) : base(n, 100) { }

        public override void Gyogyul()
        {
            if(hp < 30) Iszik(Math.Min(elixir, 30-hp));
        }
        public override int Ero() { return tul.Tul(this).e; }
        public override int Pajzs() { return tul.Tul(this).p; }
        public override int MaxHp() { return tul.Tul(this).h; }
        public override string Tipus() { return "vakmerő"; }
    }

    public class Ugyes : Elf
    {
        public Ugyes(string n) : base(n, 80) { }

        public override void Gyogyul()
        {
            Atvalt(kincs / 2);
            if (hp < 50) Iszik(Math.Min(elixir, 50 - hp));
        }
        public override int Ero() { return tul.Tul(this).e; }
        public override int Pajzs() { return tul.Tul(this).p; }
        public override int MaxHp() { return tul.Tul(this).h; }
        public override string Tipus() { return "ügyes"; }
    }

    public class Bolcs : Elf
    {
        public Bolcs(string n) : base(n, 60) { }

        public override void Gyogyul()
        {
            Atvalt(kincs);
            Iszik(elixir);
        }
        public override int Ero() { return tul.Tul(this).e; }
        public override int Pajzs() { return tul.Tul(this).p; }
        public override int MaxHp() { return tul.Tul(this).h; }
        public override string Tipus() { return "bölcs"; }
    }
}
