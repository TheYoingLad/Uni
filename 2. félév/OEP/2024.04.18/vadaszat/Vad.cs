using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace vadaszat
{
    public enum Nem { him, nőstény}

    public abstract class Vad
    {
        protected double tomeg;
        protected Nem neme;

        public Vad(double t, Nem n)
        {
            tomeg = t;
            neme = n;
        }

        public double getTomeg() { return tomeg; }
        public Nem getNeme() { return neme; }
    }

    public class Elefant : Vad
    {
        private double bal;
        private double jobb;

        public Elefant(double t, double b, double j, Nem n) : base(t, n)
        {
            bal = b;
            jobb = j;
        }

        public double getBal() { return bal; }
        public double getJobb() { return jobb; }
    }

    public class Orrszarvu : Vad
    {
        private double szarv;

        public Orrszarvu(double t, double sz, Nem n) : base(t, n)
        {
            szarv = sz;
        }

        public double getSzarv() { return szarv; }
    }

    public class Oroszlan : Vad
    {
        public Oroszlan(double t, Nem n) : base(t, n) { }
    }
}
