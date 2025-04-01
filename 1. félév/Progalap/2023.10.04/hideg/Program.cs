namespace hideg
{
    internal class Program
    {
        static void Main(string[] args)
        {
            //Deklarálás
            int lhn;
            int n;
            int[] fok;
            //Beolvasás
            Console.Write("n = ");
            int.TryParse(Console.ReadLine(), out n);
            fok = new int[n];
            for (int i = 0; i < n; i++)
            {                
                Console.Write($"{i+1}. hőm = ");
                int.TryParse(Console.ReadLine(), out fok[i]);
            }
            //Feldolgozás
            lhn = fok[0];
            for (int i = 0; i < n; i++)
            {
                if (fok[i] < lhn) lhn = fok[i];
            }
            //Kiírás
            Console.WriteLine($"A leghidegebb hőm: {lhn}");
        }
    }
}