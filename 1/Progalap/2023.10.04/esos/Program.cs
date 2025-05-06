namespace esos
{
    internal class Program
    {
        static void Main(string[] args)
        {
            //Deklarálás
            int n;
            int[] mm;
            int db;
            bool jo;
            //Beolvasás
            do
            {
                Console.Write("n = ");
                jo = int.TryParse(Console.ReadLine(), out n);
                if (!jo) Console.WriteLine("helytelen bemenet!");
            } while (!jo);
            mm = new int[n];
            for (int i = 0; i < n; i++)
            {
                do
                {
                    Console.Write("{0}. mm = ", i + 1);
                    jo = int.TryParse(Console.ReadLine(), out mm[i]);
                    if (!jo) Console.WriteLine("helytelen bemenet!");
                } while (!jo);
            }
            //Feldolgozás
            db = 0;
            for (int i = 0; i < n; i++)
            {
                if (mm[i] != 0) db++;
            }
            //Kiírás
            Console.WriteLine($"{db} darab esős nap volt.");
        }
    }
}