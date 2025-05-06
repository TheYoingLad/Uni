namespace bovelkedoLotto
{
    internal class Program
    {
        static void Main(string[] args)
        {
            //deklarálás
            int[] lsz;
            List<int> bov = new List<int>();

            beolvasas(out lsz);
            feldolgozas(lsz, bov);
            kiiras(bov);
        }

        static void beolvasas(out int[] lsz)
        {
            int n;
            //beolvasás
            Console.Write("Hány lottószám van? ");
            int.TryParse(Console.ReadLine(), out n);
            lsz = new int[n];
            for (int i = 0; i < n; i++) lsz[i] = i + 1;
        }

        static void feldolgozas(int[] lsz, List<int> bov)
        {
            int n = lsz.Length;
            //feldolgozás
            for (int i = 0; i < n; i++) if (lsz[i] < k(lsz[i])) bov.Add(lsz[i]);
        }

        static void kiiras(List<int> bov)
        {
            Console.Write("Bővelkedő számok:");
            for (int i = 0; i < bov.Count; i++) Console.Write(" {0},", bov[i]); 
        }

        static int k(int x)
        {
            int sum = 0;
            for (int i = 1; i < x; i++) if (x % i == 0) sum += i;
            return sum;
        }
    }
}