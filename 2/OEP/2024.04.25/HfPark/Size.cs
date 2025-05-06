using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HfPark
{
    public interface Size
    {
        public int Multi();
    }

    public class S : Size
    {
        private static S? instance;

        public static S Instance()
        {
            if (instance == null) instance = new S();
            return instance;
        }

        public int Multi() { return 1; }
    }

    public class M : Size
    {
        private static M? instance;

        public static M Instance()
        {
            if (instance == null) instance = new M();
            return instance;
        }

        public int Multi() { return 2; }
    }

    public class L : Size
    {
        private static L? instance;

        public static L Instance()
        {
            if (instance == null) instance = new L();
            return instance;
        }

        public int Multi() { return 3; }
    }

    public class XL : Size
    {
        private static XL? instance;

        public static XL Instance()
        {
            if (instance == null) instance = new XL();
            return instance;
        }

        public int Multi() { return 4; }
    }
}
