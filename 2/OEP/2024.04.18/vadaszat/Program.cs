using System.Diagnostics.Metrics;
using System.Globalization;

namespace vadaszat
{
    public class Program
    {
        static void Main()
        {
            Thread.CurrentThread.CurrentCulture = CultureInfo.InvariantCulture;
            try
            {
                Vadasz hunter = new("Zsolti", "1963");
                hunter.Read("input.txt");

                Console.WriteLine($"Number of the male lions: {hunter.HanyOroszlan()}");

                (bool van, double rate, _) = hunter.MaxSzarvTomeg();

                if (van)
                {
                    Console.WriteLine($"The most horn-weigth rate among the rhinos: {rate:f3}");
                }
                else
                {
                    Console.WriteLine("There are no rhinos");
                }

                if (hunter.EgyezoAgyar())
                {
                    Console.WriteLine("There exists an elephant with same length tusks.");
                }
                else
                {
                    Console.WriteLine("There is no elephant with same length tusks.");
                }
            }
            catch (System.IO.FileNotFoundException)
            {
                Console.WriteLine("Wrong file name");
            }
        }
    }
}