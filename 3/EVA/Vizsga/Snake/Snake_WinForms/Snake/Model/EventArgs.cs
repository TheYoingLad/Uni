using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Snake.Model
{
    public class TileArgs : EventArgs
    {
        public readonly Coordinate? coord;

        public TileArgs(Coordinate coord) : base()
        {
            this.coord = coord;
        }
    }

    public class StopGameArgs : EventArgs
    {
        public readonly bool inGame;

        public StopGameArgs(bool inGame) : base()
        {
            this.inGame = inGame;
        }
    }
}
