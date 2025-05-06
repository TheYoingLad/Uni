namespace kpo
{
    internal class Program
    {
        static void Main(string[] args)
        {
            string a, b;
            string e;
            bool jo = false;

            do
            {
                Console.Write("Első játékos: ");
                a = Console.ReadLine();
                jo = a == "ko" || a == "papir" || a == "ollo";
                if (!jo) Console.WriteLine("Érvénytelen válasz! (ékezet nélkül adja meg!)\n");
            } while (!jo);

            do
            {
                Console.Write("Második játékos: ");
                b = Console.ReadLine();
                jo = b == "ko" || b == "papir" || b == "ollo";
                if (!jo) Console.WriteLine("Érvénytelen válasz! (ékezet nélkül adja meg!)\n");
            } while (!jo);        
            
            if (a == "ko" && b == "papir")
            {
                e = "B nyert";
            }
            else if (a == "ko" && b == "ollo")
            {
                e = "A nyert";
            }
            else if (a == "papir" && b == "ko")
            {
                e = "A nyert";
            }
            else if (a == "papir" && b == "ollo")
            {
                e = "B nyert";
            }
            else if (a == "ollo" && b == "papir")
            {
                e = "A nyert";
            }
            else if (a == "ollo" && b == "ko")
            {
                e = "B nyert";
            }
            else
            {
                e = "döntetlen";
            }
            
            Console.WriteLine("Az eredmény: " + e);
        }
    }
}