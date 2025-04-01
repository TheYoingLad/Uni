namespace VizsgaGyakWinForms.Model
{
    public class RowOrColEventArgs : EventArgs
    {
        private int _x;
        private bool _isRow;
        public RowOrColEventArgs(int x, bool isRow)
        {
            _x = x;
            _isRow = isRow;
        }

        public int GetCoordinate => _x;
        public bool GetIsRow => _isRow;
    }
}
