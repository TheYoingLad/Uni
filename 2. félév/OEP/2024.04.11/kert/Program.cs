namespace kert
{
    internal class Program
    {
        static void Main(string[] args)
        {
            Console.Write("Parcellák száma: ");
            int n = int.Parse(Console.ReadLine());
            Kertész jozsi = new Kertész(new Kert(n));
        }
    }
}