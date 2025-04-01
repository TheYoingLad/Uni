namespace HfBevetel
{
    struct Termek
    {
        public int cikk;
        public int ar;
    }
    struct Szamla
    {
        public string nev;
        public int n;

        public Szamla(String s) { n = 0; nev = s; }
        public void hozzaad(Termek t)
        {
            n += t.ar;
        }
    }
    internal class Program
    {
        static void Main(string[] args)
        {
            StreamReader sr = new StreamReader(args[0]);
            //StreamReader sr = new StreamReader("input.txt");
            int bevet = 0;

            while (sr.Peek() != -1)
            {
                String[] line = sr.ReadLine().Split();
                Szamla sz = new Szamla(line[0]);
                for (int i = 1; i < line.Length; i += 2)
                {
                    Termek t;
                    t.cikk = int.Parse(line[i]);
                    t.ar = int.Parse(line[i + 1]);
                    sz.hozzaad(t);
                }
                bevet += sz.n;
            }
            Console.WriteLine(bevet);
        }
    }
}