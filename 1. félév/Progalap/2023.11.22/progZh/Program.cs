// Golyha Gergő
// A7MMZ1

namespace progZh
{
    internal class Program
    {
        static void Main(string[] args)
        {
            List<int> x = beolvasas();
            bool vanegyforma = feldolgozas(x);
            kiiras(vanegyforma);
        }

        static List<int> beolvasas()
        {
            List<int> x = new List<int>();
            int n;
            bool jo;
            do
            {
                Console.Write("n = ");
                jo = int.TryParse(Console.ReadLine(), out n);
                if (!jo || n < 0) Console.WriteLine("helytelen bemenet!\nn nemnegatív egész szám lehet");
            } while (!jo || n < 0);
            for (int i = 0; i < n; i++)
            {
                int seged;
                do
                {
                    Console.Write("{0}. szám = ", i + 1);
                    jo = int.TryParse(Console.ReadLine(), out seged);
                    if (!jo || seged < -10 || seged > 10) Console.WriteLine("helytelen bemenet!\na sorozat minden eleme -10 és 10 közötti egész szám lehet");
                } while (!jo || seged < -10 || seged > 10);
                x.Add(seged);
            }
            return x;
        }

        static bool feldolgozas(List<int> x)
        {
            bool vanalatta(int i)
            {
                bool van;
                int j = i + 1;
                while (j < x.Count && !(x[i] == x[j])) j++;
                van = j < x.Count;
                return van;
            }
            int i = 0;
            while (i < x.Count - 1 && !vanalatta(i)) i++;
            return i < x.Count - 1;
        }

        static void kiiras(bool vanegyforma)
        {
            Console.WriteLine("{0} egyforma szám a sorozatban", vanegyforma ? ("van") : ("nincs"));
        }
    }
}