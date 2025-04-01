using LaserPigs.Persistence;

namespace LaserPigs.Model
{
    public class StepEventArgs : EventArgs
    {
        private Coordinate _startCoord;
        private Coordinate _endCoord;

        public StepEventArgs(Coordinate startCoord, Coordinate endCoord)
        {
            _startCoord = startCoord;
            _endCoord = endCoord;
        }

        public Coordinate GetStartCoordinate { get { return _startCoord; } }
        public Coordinate GetEndCoordinate { get { return _endCoord; } }
    }
}
