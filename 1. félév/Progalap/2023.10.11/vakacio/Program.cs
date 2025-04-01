using System;

namespace vakacio
{
    internal class Program
    {
        static void Main(string[] args)
        {
            //Deklarásá
            int n;
            int[] nap;
            int ind;
            //Beolvasás
            Console.Error.Write("n = ");
            int.TryParse(Console.ReadLine(), out n);
            nap = new int[n];
            for (int i = 0; i < n; i++)
            {
                Console.Error.Write("{0}. nap: ", i + 1);
                int.TryParse(Console.ReadLine(), out nap[i]);
            }
            //Feldolgozás
            ind = 0;
            while (ind < n - 6 && nap[ind++] + nap[ind++] + nap[ind++] + nap[ind++] + nap[ind++] + nap[ind++] + nap[ind++] != 7) ind -= 6;
            //Kiírás
            Console.Error.WriteLine("{0} napot kell várni vakációig", ind - 6);
            Console.WriteLine(ind - 6);
        }
    }
}