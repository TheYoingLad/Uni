using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace kert
{
    public class Kert
    {
        private List<Parcella> parcellák;

        public Kert(int n)
        {
            parcellák = new List<Parcella>();
            for(int i = 0; i < n; i++) parcellák[i] = new Parcella();
        }

        public void Ültet(int hova, int mikor, Növényfajta mit)
        {
            parcellák[hova].Ültet(mit, mikor);
        }
        public void Arat(int hol)
        {
            parcellák[hol].Arat();
        }
        public List<int> Aratható(int hónap)
        {
            List<int> erettek = new List<int>();
            for (int i = 0; i < parcellák.Count; i++) if (parcellák[i].Beérik(hónap)) erettek.Add(i);
            return erettek;
        }
    }
    public class Parcella
    {
        private int ültetésiIdő;
        private Növényfajta? fajta;

        public void Ültet(Növényfajta növ, int mikor)
        {
            if (fajta != null) throw new InvalidOperationException();
            fajta = növ;
            ültetésiIdő = mikor;
        }
        public bool Beérik(int hónap)
        {
            return fajta != null && fajta.isZöldség() && hónap-ültetésiIdő == fajta.getÉrésiIdő();
        }
        public void Arat() { fajta = null; }
    }
}
