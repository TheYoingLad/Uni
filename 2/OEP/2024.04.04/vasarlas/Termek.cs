using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace vasarlas
{
    public class Termek
    {
        private String nev;
        private int ar;

        public String getNev() { return nev; }
        public int getAr() { return ar; }

        public Termek(String n, int a)
        {
            nev = n;
            ar = a;
        }
    }
}
