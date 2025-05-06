using System;
using System.Collections.Generic;

namespace dragaLakasok
{
    internal class Program
    {
        struct Lakas
        {
            public int ter;
            public int ar;
        }

        static void Main(string[] args)
        {
            //dekl
            int n;
            int k;
            Lakas[] lakasok;
            List<int> dragak = new List<int>();
            //be
            if (Console.IsInputRedirected)
            {

                string[] be = Console.ReadLine().Split();
                int.TryParse(be[0], out n);
                int.TryParse(be[1], out k);

                lakasok = new Lakas[n];
                for (int i = 0; i < n; i++)
                {
                    be = Console.ReadLine().Split();
                    int.TryParse(be[0], out lakasok[i].ter);
                    int.TryParse(be[1], out lakasok[i].ar);
                }
            }
            else
            {
                Console.Write("n = ");
                int.TryParse(Console.ReadLine(), out n);
                Console.Write("k = ");
                int.TryParse(Console.ReadLine(), out k);
                lakasok = new Lakas[n];
                for (int i = 0; i < n; i++)
                {
                    Console.Write("{0}. lakás területe: ", i + 1);
                    int.TryParse(Console.ReadLine(), out lakasok[i].ter);
                    Console.Write("{0}. lakás ára: ", i + 1);
                    int.TryParse(Console.ReadLine(), out lakasok[i].ar);
                }
            }
            //feld
            dragak.Clear();
            for (int i = 0; i < n; i++)
            {
                if (lakasok[i].ar > k) dragak.Add(i + 1);
            }
            //ki
            if (Console.IsOutputRedirected)
            {
                Console.Write(dragak.Count);
                foreach (var i in dragak)
                {
                    Console.Write(" " + i);
                }
            }
            else
            {
                Console.WriteLine("{0} drága lakás van:", dragak.Count);
                for (int i = 0; i < dragak.Count; i++)
                {
                    Console.WriteLine("{0}. lakás", dragak[i]);
                }
            }
        }
    }
}