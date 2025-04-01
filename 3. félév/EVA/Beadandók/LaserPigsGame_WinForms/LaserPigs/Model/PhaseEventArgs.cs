using LaserPigs.Persistence;

namespace LaserPigs.Model
{
    public class PhaseEventArgs : EventArgs
    {
        private Phase _newPhase;
        private Coordinate _c1;
        private Coordinate _c2;

        public PhaseEventArgs(Phase newPhase, Coordinate c1, Coordinate c2)
        {
            _newPhase = newPhase;
            _c1 = c1;
            _c2 = c2;
        }

        public Phase GetNewPhase { get { return _newPhase; } }
        public Coordinate GetCoordinate1 { get { return _c1; } }
        public Coordinate GetCoordinate2 { get { return _c2; } }
    }
}
