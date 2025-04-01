using System;
using System.Collections.Generic;
namespace helysegTobbMint90
{
    internal class Program
    {
        static void Main(string[] args)
        {
            int[,] madar = beolvasas();
            (int db, List<int> y) = feldolgozas(madar);
            kiiras(db, y);
        }

        static int[,] beolvasas()
        {
            int[,] madar;
            int m, n;
            Console.Error.Write("m n = ");
            string[] be1 = Console.ReadLine()!.Split(" ");
            int.TryParse(be1[0], out m);
            int.TryParse(be1[1], out n);
            madar = new int[m, n];
            for (int i = 0; i < m; i++)
            {
                Console.Error.Write("{0}. szoba madarai: ", i+1);
                string[] be2 = Console.ReadLine()!.Split(" ");
                for (int j = 0; j < n; j++)
                {
                    int.TryParse(be2[j], out madar[i, j]);
                }
            }
            return madar;
        }

        static (int db, List<int> y) feldolgozas(int[,] madar)
        {
            int db; List<int> y;
            db = 0;
            y = new List<int>();
            bool felt(int x)
            {
                bool van = false;
                int i = 0;
                while (i < madar.GetLength(1) && !((double)madar[x, i] / szum(x) > 0.9)) i++;
                van = i < madar.GetLength(1);
                return van;
            }
            int szum (int x)
            {
                int s = 0;
                for (int i = 0; i < madar.GetLength(1); i++)
                {
                    s += madar[x, i];
                }
                return s;
            }
            for (int i = 0; i < madar.GetLength(0); i++)
            {
                if (felt(i))
                {
                    db++;
                    y.Add(i+1); //0-tól való indexelés miatt a helység száma az indexe + 1
                }
            }
            return (db, y);
        }

        static void kiiras(int db, List<int> y)
        {
            Console.Error.Write("db = ");
            Console.WriteLine(db);
            Console.Error.Write("ezek a helységek: ");
            for (int i = 0; i < db; i++)
            {
                Console.Write(y[i] + " ");
            }
        }
    }
}