using System;

namespace ketazonosfigurajulap
{
    internal class Program
    {
        struct k
        {
            public string szin;
            public string ert;
        }
        static void Main(string[] args)
        {
            //Deklarásá
            k[] ks;
            int ind;
            //Beolvasás
            ks = new k[32];
            for (int i = 0; i < 32; i++)
            {
                Console.Error.Write("{0}. kártya: ", i + 1);
                string[] be = Console.ReadLine().Split();
                ks[i].szin = be[0];
                ks[i].ert = be[1];
            }
            //Feldolgozás
            for (ind = 0; ind < 32 - 1 && ks[ind].ert != ks[ind + 1].ert; ind++) { }

            //Kiírás
            if (ind != 32 - 1)
            {
                Console.Error.WriteLine("{0} és {1} azonos figurájú", ind + 1, ind + 2);
                Console.WriteLine(ind + 1);
            }
            else
            {
                Console.Error.WriteLine("Nincs azonos figura");
                Console.WriteLine(0);
            }           
        }
    }
}