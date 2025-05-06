namespace terulet
{
    internal class Program
    {
        static void Main(string[] args)
        {
            double a, b;
            double t;
            Console.Write("A téglalap 'a' oldala: ");
            double.TryParse(Console.ReadLine(), out a);
            Console.Write("A téglalap 'b' oldala: ");
            double.TryParse(Console.ReadLine(), out b);
            t = a * b;
            Console.WriteLine($" {a} és {b} oldalhosszúságú téglalap területe: {t}");
        }
    }
}