using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace vadaszat
{
    public class Trofea
    {
        private string helyszin; 
        private string datum;
        private Vad zsakmany;

        public Trofea(string h, string d, Vad zs)
        {
            helyszin = h;
            datum = d;
            zsakmany = zs;
        }

        public Vad getZsakmany() { return zsakmany; }
    }
}
