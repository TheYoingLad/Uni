namespace kaktusz
{
    public struct Kaktusz
    {
        public string nev;
        public string szin;
        public string os;
        public int meret;
    }
    internal class Program
    {
        static void Main(string[] args)
        {
            StreamReader sr = new ("cactus0.txt");
            List<Kaktusz> piros = new List<Kaktusz>();
            List<Kaktusz> mexikoi = new List<Kaktusz>();
            string? line = null;
            while ((line = sr.ReadLine()) != null)
            {
                Kaktusz seged;
                string[] input = line.Split();
                seged.nev = input[0];
                seged.szin = input[1];
                seged.os = input[2];
                seged.meret = int.Parse(input[3]);

                if (seged.szin == "piros") piros.Add(seged);
                
                if (seged.os == "Mexiko") mexikoi.Add(seged);
            }

            Console.WriteLine("pirosak:");
            foreach (var item in piros) Console.WriteLine(item.nev);

            Console.WriteLine("mexikoiak");
            foreach (var item in mexikoi) Console.WriteLine(item.nev);
        }
    }
}