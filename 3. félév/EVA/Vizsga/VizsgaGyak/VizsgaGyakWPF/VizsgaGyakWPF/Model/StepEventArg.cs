namespace VizsgaGyakWPF.Model
{
    public class StepEventArg : EventArgs
    {
        private readonly (int, int) _fromCoord;
        private readonly (int, int) _toCoord;

        public StepEventArg((int, int) fromCoord, (int, int) toCoord)
        {
            _fromCoord = fromCoord;
            _toCoord = toCoord;
        }

        public (int fromX, int fromY) GetFromCoord => _fromCoord;
        public (int toX, int toY) GetToCoord => _toCoord;
    }
}
