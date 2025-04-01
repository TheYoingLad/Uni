namespace gepkolcsonzes
{
    internal class Program
    {
        struct Kocsi
        {
            public int elso;
            public int utso;
            public string az;
        }

        static void Main(string[] args)
        {
            (Kocsi[] adatok, string keresett) = beolvasas();
            (int osszNap, int kiadottNap, int sorszam, int kulonbozo, List<string> egyszer) = feldolgozas(adatok, keresett);
            kiiras(osszNap, kiadottNap, sorszam, kulonbozo, egyszer);
        }

        static (Kocsi[], string) beolvasas()
        {
            int n;
            string keresett;
            Kocsi[] adatok;
            string[] be1 = Console.ReadLine().Split(" ");
            int.TryParse(be1[0], out n);
            adatok = new Kocsi[n];
            keresett = be1[1];
            for (int i = 0; i < n; i++)
            {
                string[] be2 = Console.ReadLine().Split(" ");
                adatok[i].elso = int.Parse(be2[0]);
                adatok[i].utso = int.Parse(be2[1]);
                adatok[i].az = be2[2];
            }
            return (adatok, keresett);
        }

        static (int, int, int, int, List<string>) feldolgozas(Kocsi[] adatok, string keresett)
        {
            //1.feladat
            int osszNap = 0;
            for (int i = 0; i < adatok.Length; i++) osszNap += adatok[i].utso - adatok[i].elso + 1; //felt szum

            //2.feladat
            int kiadottNap = 0;
            for (int i = 0; i < adatok.Length; i++) if (adatok[i].az == keresett) kiadottNap += adatok[i].utso - adatok[i].elso + 1; //felt szum

            //3.feladat
            int kiadottak(int x) //darab
            {
                int db = 0;
                for (int i = 0; i < adatok.Length; i++) if (x >= adatok[i].elso && x <= adatok[i].utso) db++;
                return db;
            }
            int maxNap = adatok[0].utso;
            for (int i = 1; i < adatok.Length; i++) if (adatok[i].utso > maxNap) maxNap = adatok[i].utso; //maxkiv
            int[] kiadott = new int[maxNap];
            for (int i = 0; i < maxNap; i++) kiadott[i] = kiadottak(i+1); //másolás
            int sorszam = 0;
            for (int i = 1; i < maxNap; i++) if (kiadott[i] > kiadott[sorszam]) sorszam = i; //maxkiv
            sorszam++;

            //4.feladat
            bool eddigNemVolt(int x) //mind
            {
                int i = 0;
                while (i < x && adatok[i].az != adatok[x].az) i++;
                return i == x;
            }
            int kulonbozo = 0;
            for (int i = 0; i < adatok.Length; i++) if (eddigNemVolt(i)) kulonbozo++; //darab

            //5.feladat
            int hanyszorVolt(string x) //darab
            {
                int db = 0;
                for (int i = 0; i < adatok.Length; i++) if (adatok[i].az == x) db++;
                return db;
            }
            List<string> egyszer = new List<string>();
            for (int i = 0; i < adatok.Length; i++) if (hanyszorVolt(adatok[i].az) == 1) egyszer.Add(adatok[i].az); //kiválogatás

            return (osszNap, kiadottNap, sorszam, kulonbozo, egyszer);
        }

        static void kiiras(int osszNap, int kiadottNap, int sorszam, int kulonbozo, List<string> egyszer)
        {
            //1.feladad
            Console.WriteLine($"#\n{osszNap}");
            //2.feladad
            Console.WriteLine($"#\n{kiadottNap}");
            //3.feladad
            Console.WriteLine($"#\n{sorszam}");
            //4.feladad
            Console.WriteLine($"#\n{kulonbozo}");
            //5.feladad
            Console.WriteLine($"#\n{egyszer.Count}");
            foreach (var item in egyszer)
            {
                Console.WriteLine(item);
            }
        }
    }
}
