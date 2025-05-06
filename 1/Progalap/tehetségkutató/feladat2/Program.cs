using System;

namespace feladat1
{
    internal class Program
    {
        static int n;
        static int k;
        static int[] ns;

        static void Main(string[] args)
        {
            string[] be1 = Console.ReadLine()!.Split();
            int.TryParse(be1[0], out n);
            int.TryParse(be1[1], out k);
            ns = new int[n];
            int[] lens = new int[n];
            string[] be2 = Console.ReadLine()!.Split();
            for (int i = 0; i < n; i++)
            {
                ns[i] = int.Parse(be2[i]);
            }
            int j;
            for (int i = 0; i < n; i++)
            {
                j = i;
                while (j < n && sum(i, j) <= k) j++;
                lens[i] = j-i;
            }
            int max = lens[0];
            for (int i = 0; i < n; i++) if (lens[i] > max) max = lens[i];
            Console.WriteLine(max==0?(-1):(max));
        }

        static int sum(int kezd, int veg)
        {
            int s = 0;
            for (int i = kezd; i <= veg; i++)
            {
                s += ns[i];
            }
            return s;
        }
    }
}
