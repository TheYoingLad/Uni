using LaserPigs.Persistence;

namespace LaserPigs.Model
{
    public class TurnEventArgs
    {
        private Coordinate _coord;
        private Direction _newDir;

        public TurnEventArgs(Coordinate coord, Direction newDir)
        {
            _coord = coord.Clone();

            if (newDir == Direction.None) throw new ArgumentException("Invalid direction", nameof(newDir));
            _newDir = newDir;
        }

        public Coordinate GetCoordinate { get { return _coord.Clone(); } }
        public Direction GetNewDirection { get { return _newDir; } }
    }
}
