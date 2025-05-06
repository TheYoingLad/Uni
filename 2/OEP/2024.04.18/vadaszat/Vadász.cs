namespace vadaszat
{
    public class Vadasz
    {
        private string nev;
        private string szulev;
        private List<Trofea> trofeak;

        public Vadasz(string n, string sz)
        {
            nev = n;
            szulev = sz;
            trofeak = new List<Trofea>();
        }

        public void Elejt(string hol, string mikor, Vad mit)
        {
            trofeak.Add(new Trofea(hol, mikor, mit));
        }
        public int HanyOroszlan()
        {
            int s = 0;
            foreach (var e in trofeak) if (e.getZsakmany() is Oroszlan o && o.getNeme() == Nem.him) s++;
            return s;
        }
        public (bool, double, Trofea?) MaxSzarvTomeg()
        {
            bool van = false;
            double max = 0;
            Trofea? t = null;
            foreach (var e in trofeak) if (e.getZsakmany() is Orrszarvu o)
                {
                    if (!van)
                    {
                        van = true;
                        max = o.getSzarv() / o.getTomeg();
                        t = e;
                    } else if (van && o.getSzarv() / o.getTomeg() > max)
                    {
                        max = o.getSzarv() / o.getTomeg();
                        t = e;
                    }
                }
            if (!van) return (false, 0, null);
            return (true, max, t);
        }
        public bool EgyezoAgyar()
        {
            bool van = false;
            foreach (var e in trofeak) if (e.getZsakmany() is Elefant E && E.getJobb() == E.getBal()) { van = true; break; }
            return van;
                
        }
        public void Read(string file)
        {
            //read dfrom file
        }
    }
}
