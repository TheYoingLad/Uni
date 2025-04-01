namespace vasarlas
{
    internal class Program
    {
        static void Main(string[] args)
        {
            Console.Write("Vasarlo text file neve: ");
            String vasarlofile = Console.ReadLine();
            Vasarlo v = new Vasarlo(vasarlofile);
            
            Console.Write("Uzlet elemiszer text file neve: ");
            String elelFile = Console.ReadLine();
            Console.Write("Uzlet muszaki text file neve: ");
            String muszFile = Console.ReadLine();
            Uzlet u = new Uzlet(elelFile, muszFile);

            v.Vasarol(u);

            Console.WriteLine("Ezeket vette:");
            foreach (var item in v.kosar) Console.WriteLine(item.getNev() + ": " + item.getAr());
        }
    }
}