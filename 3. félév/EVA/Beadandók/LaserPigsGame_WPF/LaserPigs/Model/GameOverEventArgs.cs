using LaserPigs.Persistence;

namespace LaserPigs.Model
{
    public class GameOverEventArgs
    {
        private bool? _winner;
        private Coordinate _coord1;
        private Coordinate _coord2;

        public GameOverEventArgs(bool? winner, Coordinate coord1, Coordinate coord2)
        {
            _winner = winner;
            _coord1 = coord1;
            _coord2 = coord2;
        }

        public bool? GetWinner { get { return _winner; } }
        public Coordinate GetCoord1 { get { return _coord1; } }
        public Coordinate GetCoord2 { get { return _coord2; } }
    }
}
