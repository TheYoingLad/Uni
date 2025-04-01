namespace akasztofa
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            char[] az = new char[26];
            for (int i = 0; i < 26; i++)
            {
                az[i] = (char)('a' + i);
            }
            List<char> tippek = new List<char>();
            bool jo = false;
            bool valid = false;
            bool kitalalt = false;
            int dif;
            char tipp;
            string szo = "";
            Console.WriteLine("Difficulty: easy(1) / medium (2) / hard (3)");
            do
            {
                jo = int.TryParse(Console.ReadLine(), out dif);
                valid = dif == 1 || dif == 2 || dif == 3;
                if (!jo || !valid) Console.WriteLine("Invalid!");
            } while (!jo || !valid);
            switch (dif)
            {
                case 1:
                    {
                        szo = "alma";
                        break;
                    }
                case 2:
                    {
                        szo = "vasbeton";
                        break;
                    }
                case 3:
                    {
                        szo = "feri";
                        break;
                    }
            }
            do
            {
                Console.Clear();
                Console.WriteLine("Welcome to the akasztofa!");
                printw(szo, tippek);
                printaz(az, tippek);
                tipp = tippeles(tippek);
                tippek.Add(tipp);
                kitalalt = check(szo, tippek);
            } while (!kitalalt);
            printw(szo, tippek);
            Console.WriteLine("Helyes!");
            Console.WriteLine("Tippek: {0}\nHelytelen Tippek: {1}", tippek.Count(), rossztipp(tippek, szo));
        }

        static char tippeles(List<char> eddig)
        {
            char be;
            bool jo = false;
            bool volt = true;
            do
            {
                Console.Write("Tipp: ");
                jo = char.TryParse(Console.ReadLine(), out be);
                if (!jo) Console.WriteLine("Helytelen tipp!");
                volt = eddig.Contains(be);
                if (volt) Console.WriteLine("Volt mar!");
            } while (!jo || volt);
            return be;
        }

        static bool check(string szo, List<char> eddig)
        {
            bool ki = true;
            int i = 0;
            while (i < szo.Length && ki)
            {
                ki = eddig.Contains(szo[i++]);
            }
            return ki;
        }

        static int rossztipp(List<char> halmaz, string helyes)
        {
            int n = 0;
            foreach (var x in halmaz)
            {
                if (!helyes.Contains(x)) n++;
            }
            return n;
        }

        static void printaz(char[] az, List<char> eddig)
        {
            Console.Write("Ezekre lehet tippelni: ");
            foreach (var x in az)
            {
                if (eddig.Contains(x)) Console.ForegroundColor = ConsoleColor.Blue;
                Console.Write("{0} ", x.ToString());
                Console.ResetColor();
            }
            Console.WriteLine("");
        }

        static void printw(string szo, List<char> eddig)
        {
            foreach (var x in szo)
            {
                if (eddig.Contains(x)) Console.Write("{0} ", x.ToString());
                else Console.Write("_ ");
            }
            Console.WriteLine("");
        }
    }


}