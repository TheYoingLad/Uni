namespace orzottfalak
{
    internal class Program
    {
        static void Main(string[] args)
        {
            //Deklarálás
            int n;
            int[] fal;
            int db;
            bool jo;
            //Beolvasás
            do
            {
                Console.Write("n = ");
                jo = int.TryParse(Console.ReadLine(), out n);
                jo = n <= 100 && n > 0;
                if (!jo) Console.WriteLine("helytelen bemenet! (1 és 100 közötti szám)");
            } while (!jo);
            fal = new int[n];
            for (int i = 0; i < n; i++)
            {
                do
                {
                    Console.Write("{0}. örhely = ", i + 1);
                    jo = int.TryParse(Console.ReadLine(), out fal[i]);
                    jo = fal[i] == 1 || fal[i] == 0;
                    if (!jo) Console.WriteLine("helytelen bemenet! (0 vagy 1)");
                } while (!jo);
            }
            //Feldolgozás
            db = 0;
            for (int i = 0; i < n - 1; i++)
            {
                if (fal[i] + fal[i + 1] > 0) db++;
            }
            //Kiírás
            Console.WriteLine($"{db} darad örzött fal van.");
        }
    }
}