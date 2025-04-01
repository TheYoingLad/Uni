namespace spiralFuzet
{
    public enum Tipus { sima, negyzet, vonal }
    class Fuzet
    {
        private Tipus tipus;
        private List<String> lapok;
        private int ures;

        public Spiral(int n, Tipus tip)
        {
            lapok = new List<string>();
            for (int i = 0; i < n; i++) lapok.Add("");
            tipus = tip;
            ures = n;
        }
        public int LapDB()
        {
            return lapok.Count;
        }
        public int UresDB()
        {
            return ures;
        }
        public void Rair(int ind, string tart)
        {
            if (ind < 0 || ind > lapok.Count - 1 || lapok[ind] != "") return;
            lapok[ind] = tart;
            ures--;
        }
        public void Kitep(int ind)
        {
            if (ind < 0 || ind > lapok.Count - 1) return;
            if (lapok[ind] == "") ures--;
            lapok.RemoveAt(ind);
        }
        public (bool, int) Keres()
        {
            int i = 0;
            while (i < lapok.Count && lapok[i] != "") i++;
            if (i == lapok.Count) return (false, -1);
            return (true, --i);
        }
    }

    internal class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Hello, World!");
        }
    }
}