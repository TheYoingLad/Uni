namespace VizsgaGyakWinForms.Persistence
{
    public class BlockTable
    {
        private bool[,] _data;

        public BlockTable(bool[,] data)
        {
            if (data.GetLength(0) != 4 || data.GetLength(1) != 4) throw new ArgumentException("Invalid table size");
            _data = new bool[4, 4];
            for (int i = 0; i < 4; i++)
            {
                for (int j = 0; j < 4; j++)
                {
                    _data[i, j] = data[i, j];
                }
            }
        }
        public BlockTable()
        {
            _data = new bool[4, 4];
            for (int i = 0; i < 4; i++)
            {
                for (int j = 0; j < 4; j++)
                {
                    _data[i, j] = false;
                }
            }
        }

        public bool GetData(int x, int y)
        {
            if (x < 0 || x > 3) throw new ArgumentException("Invalid x coordinate");
            if (y < 0 || y > 3) throw new ArgumentException("Invalid y coordinate");
            return _data[x, y];
        }
        public void SetData(int x, int y, bool b)
        {
            if (x < 0 || x > 3) throw new ArgumentException("Invalid x coordinate");
            if (y < 0 || y > 3) throw new ArgumentException("Invalid y coordinate");
            _data[x, y] = b;
        }
    }
}
