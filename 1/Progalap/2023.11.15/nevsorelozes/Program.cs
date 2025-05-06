namespace nevsorelozes
{
    internal class Program
    {
        static void Main(string[] args)
        {
            int[] sor;
            int maxind;

            sor = beolvasas();
            maxind = feldolgozas(sor);
            kiiras(maxind);
        }
        static int[] beolvasas()
        {
            int[] sor;
            int n;
            bool jo;
            do
            {
                Console.Error.Write("n = ");
                jo = int.TryParse(Console.ReadLine(), out n);
                if (!jo || n < 1) Console.Error.WriteLine("helytelen bemenet!");
            } while (!jo || n < 1);
            sor = new int[n];
            for (int i = 0; i < n; i++)
            {
                do
                {
                    Console.Error.Write("{0}. magasság = ", i + 1);
                    jo = int.TryParse(Console.ReadLine(), out sor[i]);
                    if (!jo || sor[i] < 1) Console.Error.WriteLine("helytelen bemenet!");
                } while (!jo || sor[i] < 1);
            }
            return sor;
        }

        static int feldolgozas(int[] sor)
        {
            int magasabb(int x)
            {
                int db = 0;
                for (int i = 0; i < x; i++)
                {
                    if (sor[i] > sor[x]) db++;
                }
                return db;
            }
            int maxért = magasabb(0);
            int maxind = 0;
            for (int i = 0; i < sor.Length; i++)
            {
                if (magasabb(i) > maxért)
                {
                    maxért = magasabb(i);
                    maxind = i;
                }
            }
            return maxind;
        }

        static void kiiras(int maxind)
        {
            Console.Error.Write("elotte volt a legtobb ember: ");
            Console.WriteLine(maxind+1);
        }
    }
}