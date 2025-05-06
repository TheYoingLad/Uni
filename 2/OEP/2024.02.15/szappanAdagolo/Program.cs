namespace szappanAdagolo
{
    class Adagolo
    {
        private int tele;
        private int akt;
        private int adag;

        public int GetTele() { return tele; }
        public int Tele { get { return tele; } }
        public int GetAkt() { return akt; }
        public int GetAdag() { return adag; }

        public Adagolo(int tele, int adag)
        {
            this.tele = tele;
            this.akt = 0;
            this.adag = adag;
        }

        public void Nyom()
        {
            akt = Math.Max(akt - adag, 0);
        }
        public void Feltölt()
        {
            akt = tele;
        }
    }

    internal class Program
    {
        static void Main(string[] args)
        {
            Adagolo a = new Adagolo(100, 30);
            a.Feltölt();
            a.Nyom();
            a.Nyom();
            Console.WriteLine(a.GetAkt());
        }
    }
}