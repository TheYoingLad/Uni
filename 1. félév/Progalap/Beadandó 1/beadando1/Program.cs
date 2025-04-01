using System;

// Golyha Gergő
// A7MMZ1
// golyhagergo@gmail.com
// Maximumkiválasztás: Legidősebb ember

namespace beadando1
{
    internal class Program
    {
        struct Kor
        {
            public int ev;
            public int ho;
            public int nap;
        }
        static void Main(string[] args)
        {
            ///////////////////////////// Deklarálás ///////////////////////////////
            
            int n;
            Kor[] kor;
            int ind;


            ///////////////////////////// Beolvasás ////////////////////////////////
            
            Console.Error.Write("n = ");
            // Előfeltétel: 1 <= n
            int.TryParse(Console.ReadLine(), out n);
            kor = new Kor[n];
            for (int i = 0; i < n; i++)
            {
                Console.Error.Write("{0}. adat (év hónap nap): ", i + 1);
                string[] be = Console.ReadLine().Split(" ");
                int.TryParse(be[0], out kor[i].ev);
                int.TryParse(be[1], out kor[i].ho);
                int.TryParse(be[2], out kor[i].nap);
            }

            
            //////////////////////////// Feldolgozás ///////////////////////////////

            // A feladat leírása miatt a legnagyobb megadható dátum a 2014.12.30, valamit minden év 12 hónapos és minden hónap 30 napos.
            // Számoljuk ki, hogy ki milyen idős ehhez a maximális időpillanathoz képest, tehát a dátumok különbségét.
            // A számolásokat napokkal végezzük el, mert itt ez a legkisebb időegység.
            // A különbségek közül kell kiválasztani a maximumot, azaz a legtöbb napos embert, azaz a legidősebbet.

            // Alkalmazzuk a maximumkiválasztás sablonját!

            int maxért = (2014 - kor[0].ev) * 12 * 30 + (12 - kor[0].ho) * 30 + (30 - kor[0].nap);
            // maxért segédváltozó: szükésges a maximumkiválasztáshoz de a feladat nem kéri mint kimenet
            ind = 0;
            // az indexelés 0-tól kezdődik
            for (int i = 1; i < n; i++)
            {
                if ((2014 - kor[i].ev) * 12 * 30 + (12 - kor[i].ho) * 30 + (30 - kor[i].nap) > maxért)
                {
                    maxért = (2014 - kor[i].ev) * 12 * 30 + (12 - kor[i].ho) * 30 + (30 - kor[i].nap);
                    ind = i;
                }
            }


            //////////////////////////////// Kiírás ////////////////////////////////
            
            Console.Error.Write("A legidősebb ember sorszáma: ");
            // a 0-tól induló indexelés miatt 1-et hozzá kell adni a kapott sorszámhoz, így kapjuk meg a végső eredményt
            Console.Write(ind + 1);
        }
    }
}