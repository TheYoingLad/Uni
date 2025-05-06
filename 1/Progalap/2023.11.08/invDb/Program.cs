using System.Threading.Channels;

namespace invDb
{
    internal class Program
    {
        static void Main(string[] args)
        {
            int[,] madarak = beolvasas();
            (bool van, int helyseg) = kereses(madarak);
            kiiras(van, helyseg);
        }
        static int[,] beolvasas()
        {
            int[,] madarak;
            #region regi
            int m; int n;
            string[] be = Console.ReadLine()!.Split();
            int.TryParse(be[0], out m);
            int.TryParse(be[1], out n);
            madarak = new int[m, n];
            for (int i = 0; i < m; i++)
            {
                string[] be2 = Console.ReadLine()!.Split();
                for (int j = 0; j < n; j++) int.TryParse(be2[j], out madarak[i, j]);
            }
            #endregion
            return madarak;
        }

        static (bool van, int helyseg) kereses(int[,] madarak)
        {
            bool van; int helyseg;
            #region regi
            bool csaknulla(int x)
            {
                int n = madarak.GetLength(1);
                int i = 0;
                while (i < n && madarak[x, i] == 0) i++;
                return i == n;
            }

            int m = madarak.GetLength(0);
            int i = 0;
            helyseg = 0;
            while (i < m && !csaknulla(i)) i++;
            van = i < m;
            if (van) helyseg = i;
            #endregion
            return (van, helyseg);
        }


        static void kiiras(bool van, int helyseg)
        {
            Console.WriteLine(van ? helyseg : 0);
        }
    }
}