namespace osszeso
{
    internal class Program
    {
        static void Main(string[] args)
        {
            //Deklarálás
            int n;
            int[] mm;
            int sum;
            bool jo;
            //Beolvasás
            do
            {
                Console.Write("n = ");
                jo = int.TryParse(Console.ReadLine(), out n);
                jo = n >= 0 && n <= 100;
                if (!jo) Console.WriteLine("helytelen bemenet! (0 és 100 közötti szám)");
            } while (!jo);
            mm = new int[n];
            for (int i = 0; i < n; i++)
            {
                do
                {
                    Console.Write("{0}. mm = ", i + 1);
                    jo = int.TryParse(Console.ReadLine(), out mm[i]);
                    jo = mm[i] >= 0;
                    if (!jo) Console.WriteLine("helytelen bemenet! (nemnegatív szám)");
                } while (!jo);
            }
            //Feldolgozás
            sum = 0;
            for (int i = 0; i < n; i++) sum += mm[i];
            //Kiírás
            Console.WriteLine($"{sum} mm esö esett.");
        }
    }
}