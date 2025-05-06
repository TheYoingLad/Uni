using System.Globalization;
namespace HfHomerseklet
{
    internal class Program
    {
        static void Main(string[] args)
        {
            Thread.CurrentThread.CurrentCulture = CultureInfo.CreateSpecificCulture("en-US");
            StreamReader sr = new StreamReader(args[0]);
            double a;
            bool l;
            double small;
            double s = 0;
            int db = 0;
            double seged;
            while ((seged = Double.Parse(sr.ReadLine()!)) >= 0)
            {
                db++;
                s += seged;
            }
            a = s/ db;
            l = true;
            small = seged;
            string line;
            while ((line = sr.ReadLine()) != null)
            {
                seged = Double.Parse(line);
                l = l && seged < 0;
                if (seged < small) small = seged;
            }
            Console.WriteLine(a);
            Console.WriteLine(l);
            Console.WriteLine(small);
        }
    }
}