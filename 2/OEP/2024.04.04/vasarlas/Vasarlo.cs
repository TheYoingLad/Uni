using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace vasarlas
{
    public class Vasarlo
    {
        private String nev;
        private List<String> bevasarlo_lista;
        public List<Termek> kosar;

        public Vasarlo(String filename)
        {
            bevasarlo_lista = new List<string>();
            kosar = new List<Termek>();
            StreamReader sr = new StreamReader(filename);
            nev = sr.ReadLine();
            while (sr.Peek() != -1) bevasarlo_lista.Add(sr.ReadLine());
        }

        public void Vasarol(Uzlet u)
        {
            foreach(var nev in bevasarlo_lista)
            {
                (bool l, Termek t) = Keres(nev, u.elelmiszer);
                if (l) Vesz(t, u.elelmiszer);
            }
            foreach (var nev in bevasarlo_lista)
            {
                (bool l, Termek t) = OlcsotKeres(nev, u.muszaki);
                if (l) Vesz(t, u.muszaki);
            }
        }

        private static (bool, Termek)Keres(String nev, Reszleg r)
        {
            int i = 0;
            while (i < r.keszlet.Count() && r.keszlet[i].getNev() != nev) i++;
            if (i < r.keszlet.Count()) return (true, r.keszlet[i]);
            else return (false, null);
        }

        private static (bool, Termek) OlcsotKeres(String nev, Reszleg r)
        {
            bool l = false;
            int minind = 0;
            for (int i = 0; i < r.keszlet.Count(); i++)
            {
                if(r.keszlet[i].getNev() == nev)
                {
                    if (!l)
                    {
                        l = true;
                        minind = i;
                    }
                    else if (r.keszlet[i].getAr() < r.keszlet[minind].getAr()) minind = i;
                }
            }
            if (l) return (true, r.keszlet[minind]);
            else return (false, null);
        }

        private void Vesz(Termek t, Reszleg r)
        {
            r.keszlet.Remove(t);
            kosar.Add(t);
        }
    }
}
