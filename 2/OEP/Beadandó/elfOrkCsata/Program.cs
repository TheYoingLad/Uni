namespace elfOrkCsata
{
    public class Program
    {
        static void Main(string[] args)
        {
            if (args.Length < 1) { throw new ArgumentException("Nincs paraméter megadva (fájlnév)"); }
            try
            {
                Csata cs = new Csata(args[0]);
                cs.Szimulal();
            } catch (Exception ex)
            {
                Console.WriteLine("Hiba lépett fel a fájl beolvasása közben:\n" + ex.Message);
            }
        }
    }
}
