using VizsgaGyakWinForms.Persistence;

namespace VizsgaGyakWinForms.Model
{
    public class BlockModel
    {
        private BlockTable _table;
        private IFileManager _fileManager;
        private int _points;
        private int _blockId;
        private Random _random;

        public BlockModel(IFileManager fileManager)
        {
            _fileManager = fileManager;
            _table = new BlockTable();
            _points = 0;
            _random = new Random();
            _blockId = 0;
        }

        public event EventHandler<CellEventArgs>? CellChanged;
        public event EventHandler<RowOrColEventArgs>? RowOrColChanged;
        public event EventHandler<(int, int)>? PointsAndBlockChanged;

        public void Save(string path)
        {
            _fileManager.Save(path, _table, _points);
        }

        public void Load(string path)
        {
            (_table, _points) = _fileManager.Load(path);
        }
        public void Place(int x, int y)
        {
            List<(int, int)> coords = Block.GetBlockCoords(_blockId);
            foreach ((int x, int y) coord in coords)
            {
                if (x + coord.x < 0 || x + coord.x > 3) throw new ArgumentException("Invalid x coordinate");
                if (y + coord.y < 0 || y + coord.y > 3) throw new ArgumentException("Invalid y coordinate");
                if (_table.GetData(x + coord.x, y + coord.y)) throw new ArgumentException("Cell already set to given value");
            }

            foreach ((int x, int y) coord in coords)
            {
                _table.SetData(x + coord.x, y + coord.y, true);
                OnCellChanged(x + coord.x, y + coord.y);
            }
            CheckRowsCols();

            _blockId = _random.Next(0, 4);
            _points++;
            OnPointsAndBlockChanged();
        }



        private void CheckRowsCols()
        {
            List<int> rows = new List<int>();
            List<int> cols = new List<int>();

            for (int i = 0; i < 4; i++)
            {
                bool row = true;
                for (int j = 0; j < 4; j++) row = row && _table.GetData(i, j);
                if (row) rows.Add(i);
            }
            for (int j = 0; j < 4; j++)
            {
                bool col = true;
                for (int i = 0; i < 4; i++) col = col && _table.GetData(i, j);
                if (col) cols.Add(j);
            }

            foreach (int i in rows)
            {
                for (int j = 0; j < 4; j++) _table.SetData(i, j, false);
                OnRowOrColChanged(i, true);
            }
            foreach (int j in cols)
            {
                for (int i = 0; i < 4; i++) _table.SetData(i, j, false);
                OnRowOrColChanged(j, false);
            }
        }


        private void OnCellChanged(int x, int y)
        {
            CellChanged?.Invoke(this, new CellEventArgs(x, y));
        }
        private void OnRowOrColChanged(int x, bool isRow)
        {
            RowOrColChanged?.Invoke(this, new RowOrColEventArgs(x, isRow));
        }
        private void OnPointsAndBlockChanged()
        {
            PointsAndBlockChanged?.Invoke(this, (_points, _blockId));
        }
    }
}
