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
            _c1 = c1.Clone();
            _c2 = c2.Clone();
        }

        public Phase GetNewPhase { get { return _newPhase; } }
        public Coordinate GetCoordinate1 { get { return _c1.Clone(); } }
        public Coordinate GetCoordinate2 { get { return _c2.Clone(); } }
    }
}
