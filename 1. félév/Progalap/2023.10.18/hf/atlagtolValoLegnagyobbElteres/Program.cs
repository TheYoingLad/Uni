namespace atlagtolValoLegnagyobbElteres
{
    internal class Program
    {
        static void Main(string[] args)
        {
            //deklarálás
            int n;
            double[] tomb;
            double max;
            double sum;
            //beolvasás
            Console.Write("n = ");
            n = int.Parse(Console.ReadLine());
            tomb = new double[n];
            for (int i = 0; i < n; i++)
            {
                Console.Write("{0}. elem: ", i + 1);
                tomb[i] = double.Parse(Console.ReadLine());
            }
            //feldolgozás
            sum = 0;
            for (int i = 0; i < n; i++) sum += tomb[i];
            max = Math.Abs(sum / n - tomb[0]);
            for (int i = 0; i < n; i++)
                if (Math.Abs(sum / n - tomb[i]) > max)
                    max = Math.Abs(sum / n - tomb[i]);
            //kiírás
            Console.WriteLine("Az átlagtól való legnagyobb eltérés: {0}", max);
        }
    }
}