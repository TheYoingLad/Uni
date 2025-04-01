using System;

namespace nevsornovekvo
{
    internal class Program
    {
        struct d
        {
            public string nev;
            public int mag;
        }
        static void Main(string[] args)
        {
            //Deklarásá
            d[] ds;
            int n;
            bool jo;
            //Beolvasás
            Console.Error.Write("n = ");
            int.TryParse(Console.ReadLine(), out n);
            ds = new d[n];
            for (int i = 0; i < n; i++)
            {
                Console.Error.Write("{0}. diák: ", i + 1);
                string[] be = Console.ReadLine().Split();
                ds[i].nev = be[0];
                int.TryParse(be[1], out ds[i].mag);
            }
            //Feldolgozás
            {
                int i;
                for (i = 1; i < n && ds[i].mag >= ds[i - 1].mag; i++) { }
                jo = i == n;
            }
            //Kiírás
            if (jo)
            {
                Console.Error.WriteLine("Helyes a sorrend");
                Console.WriteLine("IGEN");
            }
            else
            {
                Console.Error.WriteLine("Helytelen sorrend");
                Console.WriteLine("NEM");
            }               
        }
    }
}