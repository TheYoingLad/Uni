using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Grid
{
    public class GridButton : Button
    {
        private int _x;
        private int _y;


        public int GridX { get { return _x; } }
        public int GridY { get { return _y; } }

        public GridButton(int x, int y)
        {
            _x = x;
            _y = y;
        }
    }
}
