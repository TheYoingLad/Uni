namespace VizsgaGyakWinForms.Model
{
    public class CellEventArgs : EventArgs
    {
        private int _x, _y;
        
        public CellEventArgs(int x, int y)
        {
            _x = x;
            _y = y;
        }

        public (int, int) GetCoordinate => (_x, _y);
    }
}
