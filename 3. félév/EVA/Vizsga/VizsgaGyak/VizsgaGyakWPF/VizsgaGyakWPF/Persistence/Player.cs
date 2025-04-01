namespace VizsgaGyakWPF.Persistence
{
    public class Player
    {
        private Piece[] _pieces;
        private readonly bool _isP1;
        private readonly int _size;

        public Player(int size, bool isP1)
        {
            if (size != 4 && size != 6 && size != 8) throw new ArgumentOutOfRangeException("Invalid table size");
            _size = size;

            _isP1 = isP1;

            _pieces = new Piece[4];
            GenerateTable();
        }

        public (int, int) this[int i] => (_pieces[i].GetX, _pieces[i].GetY);




        public void Kill((int, int) coord)
        {
            for (int i = 0; i < 4; i++) if (this[i] == coord) _pieces[i].Kill();
        }
        public bool IsUnOccupied((int, int) coord)
        {
            for (int i = 0; i < 4; i++) if (this[i] == coord) return false;
            return true;
        }
        public bool IsAlive(int i)
        {
            return _pieces[i].IsAlive;
        }
        public string Text(int x, int y)
        {
            string text = "";
            for (int i = 0; i < 4; i++)
            {
                if (this[i] == (x, y)) text = (i + 1).ToString();
            }
            return text;
        }


        private void GenerateTable()
        {
            if (_isP1)
            {
                _pieces[0] = new Piece(1, _size - 1);
                _pieces[1] = new Piece(1, _size - 2);
                _pieces[2] = new Piece(0, _size - 2);
                _pieces[3] = new Piece(0, _size - 1);
            }
            else
            {
                _pieces[0] = new Piece(_size - 2, 0);
                _pieces[1] = new Piece(_size - 2, 1);
                _pieces[2] = new Piece(_size - 1, 1);
                _pieces[3] = new Piece(_size - 1, 0);
            }
        }
    }
}
