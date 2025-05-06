using System;

// Golyha Gergő
// A7MMZ1
// golhyagergo@gmail.com
// Tételek összeépíése: Gazdaságos üzemeltetés

namespace beadando2
{
    internal class Program
    {
        struct Utas
        {
            public int le;
            public int fel;
        }

        static void Main(string[] args)
        {
            (int u, int v, Utas[] all) = beolvasas();
            bool gazd = feldolgozas(u, v, all);
            kiiras(gazd);
        }

        static (int u, int v, Utas[] all) beolvasas()
        {
            int u, v;
            int n;
            Utas[] all;
            Console.Error.Write("n = ");
            // Ef: 1 <= n <= 100
            int.TryParse(Console.ReadLine(), out n);
            all = new Utas[n];
            Console.Error.Write("u = , v = ");
            string[] be1 = Console.ReadLine().Split();
            int.TryParse(be1[0], out u);
            int.TryParse(be1[1], out v);
            // Ef: 0 <= u <= 100
            //     0 <= v <= 100000
            for (int i = 0; i < n; i++)
            {
                Console.Error.Write("{0}. állomáson le = , fel = ");
                string[] be2 = Console.ReadLine().Split();
                int.TryParse(be2[0], out all[i].le);
                int.TryParse(be2[1], out all[i].fel);
                // Ef: 0 <= le  <= 800
                //     0 <= fel <= 800
            }
            // Ef: all[0].le    == 0
            //     all[n-1].fel == 0
            return (u, v, all);
        }

        static bool feldolgozas(int u, int v, Utas[] all)
        {
            int vonaton(int x)
            {
                int s = 0;
                for (int i = 0; i < x; i++)
                {
                    s += all[i].fel - all[i].le;
                }
                return s;
            }

            bool gazd;
            int s = 0;
            for (int i = 0; i < all.Length - 1; i++)
            {
                s += vonaton(i);
            }
            return s * u > all.Length * v;
        }

        static void kiiras(bool gazd)
        {
            Console.Error.WriteLine("Az üzemeltetés {0} gazdaságos", gazd ? "" : "nem");
            Console.WriteLine(gazd ? 1 : 0);
        }
    }
}