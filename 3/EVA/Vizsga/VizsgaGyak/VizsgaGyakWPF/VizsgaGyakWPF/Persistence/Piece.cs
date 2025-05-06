namespace VizsgaGyakWPF.Persistence
{
    public class Piece
    {
        private int _x;
        private int _y;
        private bool _isAlive;

        public Piece(int x, int y)
        {
            _x = x;
            _y = y;
            _isAlive = true;
        }

        public int GetX => _x;
        public int GetY => _y;
        public bool IsAlive => _isAlive;

        public void Kill()
        {
            _isAlive = false;
        }
        public void StepTo(int x, int y)
        {
            _x = x;
            _y = y;
        }
    }
}
