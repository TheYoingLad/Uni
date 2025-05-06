using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace elfOrkCsata
{
    public class Csata
    {
        private List<Elf> elfek;
        private List<Ork> orkok;

        public Csata(string filename)
        {
            elfek = new List<Elf>();
            orkok = new List<Ork>();
            StreamReader sr = new StreamReader(filename);

            while (sr.Peek() != -1)
            {
                string[] be = sr.ReadLine()!.Split(' ');
                if (be.Length == 4)
                {
                    switch (be[2])
                    {
                        case "v":
                            {
                                Ork o = new Verengzo(be[0], int.Parse(be[3]));
                                orkok.Add(o);
                                break;
                            }
                        case "r":
                            {
                                Ork o = new Ravasz(be[0], int.Parse(be[3]));
                                orkok.Add(o);
                                break;
                            }
                        case "o":
                            {
                                Ork o = new Ovatos(be[0], int.Parse(be[3]));
                                orkok.Add(o);
                                break;
                            }
                    }
                }
                else
                {
                    switch (be[2])
                    {
                        case "v":
                            {
                                Elf e = new Vakmero(be[0]);
                                elfek.Add(e);
                                break;
                            }
                        case "u":
                            {
                                Elf e = new Ugyes(be[0]);
                                elfek.Add(e);
                                break;
                            }
                        case "b":
                            {
                                Elf e = new Bolcs(be[0]);
                                elfek.Add(e);
                                break;
                            }
                    }
                }
            }
            sr.Close();
            if (elfek.Count == 0 || orkok.Count == 0) throw new Exception("Valamelyik sereg üres!");
        }
        public Csata(List<Elf> e, List<Ork> o)
        {
            if (e.Count == 0 || o.Count == 0) throw new Exception("Valamelyik sereg üres!");
            elfek = new List<Elf>();
            foreach (Elf elf in e) if (!elfek.Contains(elf)) elfek.Add(elf);
            orkok = new List<Ork>();
            foreach (Ork ork in o) if (!orkok.Contains(ork)) orkok.Add(ork);
        }

        //a következő két metódus a teszteléshez szükséges, a feladatban nem vesznek részt, ezért használom az egyszerű return megoldást a korrekt mély másolás helyett, ami betartaná az open-closed elveket 
        public List<Elf> getElfek() { return elfek; } 
        public List<Ork> getOrkok() { return orkok; }

        public void Szimulal()
        {
            while (elfek.Count > 0 && orkok.Count > 0) Menet();
        }
        public void Menet()
        {
            if (elfek.Count == 0) throw new ArgumentException("Nincs már élő elf a csatában!");
            int i = 0, j = 0;
            while (i < elfek.Count && orkok.Count > 0)
            {
                j %= orkok.Count;
                (bool eel, bool oel) = Kuzd(elfek[i], orkok[j]);
                if (eel)
                {
                    elfek[i].Gyogyul();
                    i++;
                }
                if (oel) j++;
            }
            Show();
        }
        public (bool eel, bool oel) Kuzd(Elf elf, Ork ork)
        {
            if (!elfek.Contains(elf) || !orkok.Contains(ork)) throw new ArgumentOutOfRangeException("Legalább az egyik harcos nem része a csatának!");
            bool halott = elf.Tamad(ork);
            if (halott)
            {
                orkok.Remove(ork);
                return (true, false);
            }
            halott = ork.Tamad(elf);
            if (halott)
            {
                elfek.Remove(elf);
                return (false, true);
            }
            return (true, true);
        }
        public void Show()
        {
            Console.WriteLine("Elfek:");
            foreach (Elf elf in elfek)
            {
                Console.Write(elf.getNev() + ": ");
                Console.Write(elf.Tipus() + ", ");
                Console.Write("hp: " + elf.getHp() + ", ");
                Console.Write("elixir: " + elf.getElixir() + ", ");
                Console.Write("kincs: " + elf.getKincs() + "\n");
            }
            Console.WriteLine("\nOrkok:");
            foreach (Ork ork in orkok)
            {
                Console.Write(ork.getNev() + ": ");
                Console.Write(ork.Tipus() + ", ");
                Console.Write("hp: " + ork.getHp() + ", ");
                Console.Write("kincs: " + ork.getKincs() + "\n");
            }
            Console.WriteLine("------------");
        }
    }
}
