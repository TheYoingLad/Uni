using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace vasarlas
{
    public class Uzlet
    {
        public Reszleg elelmiszer;
        public Reszleg muszaki;

        public Uzlet(String e, String m)
        {
            elelmiszer = new Reszleg(e);
            muszaki = new Reszleg(m);
        }
    }
}
