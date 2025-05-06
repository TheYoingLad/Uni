using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace elfOrkCsata
{
    public class Tulajdonsag
    {
        private static Tulajdonsag? instance;

        public static Tulajdonsag Instance()
        {
            if (instance == null) instance = new Tulajdonsag();
            return instance;
        }

        public (int e, int p, int h) Tul(Vakmero h) { return (30, 10, 100); }
        public (int e, int p, int h) Tul(Ugyes h) { return (10, 20, 80); }
        public (int e, int p, int h) Tul(Bolcs h) { return (10, 10, 60); }
        public (int e, int p, int h) Tul(Verengzo h) { return (30, 5, 100); }
        public (int e, int p, int h) Tul(Ravasz h) { return (15, 20, 90); }
        public (int e, int p, int h) Tul(Ovatos h) { return (10, 15, 80); }
    }
}
