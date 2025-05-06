namespace primSznap
{
    internal class Program
    {
        static void Main(string[] args)
        {
            //deklaráció
            int n;
            int[] napok;
            int db;
            //beolvasás
            Console.Write("n = ");
            int.TryParse(Console.ReadLine(), out n);
            napok = new int[n];
            for (int i = 0; i < n; i++)
            {
                Console.Write("{0}. szülinap: ");
                int.TryParse(Console.ReadLine(), out napok[i]);
            }
            //feldolgozás
            db = 0;
            for (int i = 0; i < n; i++)
            {
                if (prime(napok[i])) db++;
            }
            //kiírás
            Console.WriteLine("{0} darab prym szülinap van", db);
        }

        static bool prime(int szn)
        {
            int i = 2;
            while (i < szn - 1 && i % szn != 0) i++;
            bool mind = i > szn - 1;
            return mind;
        }
    }
}