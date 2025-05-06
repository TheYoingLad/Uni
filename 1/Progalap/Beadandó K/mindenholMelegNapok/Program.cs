/* 
    Készítette: Golyha Gergő
    Neptun: A7MMZ1
    E-mail: golhyagergo@gmail.com
    Feladat: Időjárás előrejelzés: Mindenhol meleg napok
*/

using System;
using System.Collections.Generic;

namespace mindenholMelegNapok
{
    internal class Program
    {
        static void Main(string[] args)
        {
            //deklarásál bemenet
            int[,] idojaras;

            //deklarálás kimenet
            List<int> y;

            idojaras = beolvasas();

            y = feldolgozas(idojaras);

            kiiras(y);
        }

        static int[,] beolvasas()
        {
            int[,] idojaras;
            if (Console.IsInputRedirected) idojaras = beolvasas_biro();
            else idojaras = beolvasas_kezi();
            return idojaras;
        }

        static int[,] beolvasas_kezi()
        {
            int n, m;
            int[,] idojaras;
            bool jo;
            do
            {
                Console.Write("Települések száma: ");
                jo = int.TryParse(Console.ReadLine(), out n) && n >= 1 && n <= 1000;
                if (!jo) Console.WriteLine("Egész szám kell 1 és 1000 között!");
            } while (!jo);
            do
            {
                Console.Write("Napok száma: ");
                jo = int.TryParse(Console.ReadLine(), out m) && m >= 1 && m <= 1000;
                if (!jo) Console.WriteLine("Egész szám kell 1 és 1000 között!");
            } while (!jo);
            idojaras = new int[n, m];
            for (int i = 0; i < n; i++)
            {
                for (int j = 0; j < m; j++)
                {
                    do
                    {
                        Console.Write("{0}. település {1}. napi legmagasabb hőmérsékelte: ", i + 1, j + 1);
                        jo = int.TryParse(Console.ReadLine(), out idojaras[i, j]) && idojaras[i, j] >= -50 && idojaras[i, j] <= 50;
                        if (!jo) Console.WriteLine("Egész szám kell -50 és 50 között!");
                    } while (!jo);
                }
            }
            return idojaras;
        }

        static int[,] beolvasas_biro()
        {
            int n, m;
            int[,] idojaras;
            string[] be1 = Console.ReadLine().Split(" ");
            n = int.Parse(be1[0]);
            m = int.Parse(be1[1]);
            idojaras = new int[n, m];
            for (int i = 0; i < n; i++)
            {
                string[] be2 = Console.ReadLine().Split(" ");
                for (int j = 0; j < m; j++) idojaras[i, j] = int.Parse(be2[j]);
            }
            return idojaras;
        }

        static List<int> feldolgozas(int[,] idojaras)
        {
            bool folott(int x)
            {
                bool mind;
                int i = 0;
                while (i < idojaras.GetLength(0) && idojaras[i, x] > 0) i++;
                mind = i == idojaras.GetLength(0);
                return mind;
            }

            List<int> y = new List<int>();
            for (int i = 0; i < idojaras.GetLength(1); i++) if (folott(i)) y.Add(i + 1);
            return y;
        }

        static void kiiras(List<int> y)
        {
            if (Console.IsOutputRedirected)
            {
                Console.Write(y.Count);
                foreach (var item in y) Console.Write($" {item}");
            }
            else
            {
                if (y.Count == 0)
                {
                    Console.WriteLine("\nNincs olyan nap, amikor mindenhol 0 foknál melegebb van");
                }
                else
                {
                    Console.Write($"\nMindenhol 0 foknál melegebb napok száma: {y.Count}\nEzeknek sorszáma:");
                    foreach (var item in y) Console.Write($" {item}");
                }                
                Console.WriteLine("\n\nKérem, nyomjon ENTER-t a folytatáshoz!");
                Console.ReadLine();
            }
        }
    }
}
