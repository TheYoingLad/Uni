namespace tipp
{
    internal class Program
    {
        static void Main(string[] args)
        {
            int n, t;
            bool e;
            bool jo = false;
            Random random = new Random();
            n = random.Next(1, 1000);
            do
            {
                Console.Write("Tipp: ");
                jo = int.TryParse(Console.ReadLine(), out t);
                if (!jo) Console.WriteLine("Érvénytelen tipp!");
                else if (n < t) Console.WriteLine("Helytelen tipp!\nA tipp túl nagy!");
                else if (n > t) Console.WriteLine("Helytelen tipp!\nA tipp túl kicsi!");
            } while (n != t || !jo);
            Console.WriteLine($"Helyes!\nA gondolt szám tényleg {n} volt!");
        }
    }
}