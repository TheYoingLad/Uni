using VizsgaGyakWPF.Persistence;

namespace VizsgaGyakWPF.Model
{
    public class AttackModel
    {
        private Player _p1;
        private Player _p2;
        private int _turn;
        private bool _isP1Turn;
        private int _size;

        public AttackModel(int size)
        {
            _p1 = new Player(size, true);
            _p2 = new Player(size, false);
            _turn = 0;
            _isP1Turn = true;
            _size = size;
        }

        public event EventHandler<StepEventArg>? PlayerStepped;
        public event EventHandler<bool>? GameOver;

        public int Size => _size;
        public int CurrentPlayer => _isP1Turn ? 1 : 2;
        public int CurrentPiece => _turn + 1;
        public string this[int x, int y] => !_p1.IsUnOccupied((x, y)) ? _p1.Text(x, y) : _p2.Text(x, y);

        public void Step((int, int) toCoord)
        {
            List<(int, int)> validCoords = GetValidCoordinates();
            if (!validCoords.Contains(toCoord)) throw new ArgumentException("Invalid step");

            if (_isP1Turn) _p2.Kill(toCoord);
            else _p1.Kill(toCoord);

            (int, int) fromCoord = _isP1Turn ? _p1[_turn] : _p2[_turn];
            do
            {
                if (_isP1Turn) _isP1Turn = false;
                else
                {
                    _isP1Turn = true;
                    _turn = (_turn + 1) % 4;
                }
            } while (!((_isP1Turn && _p1.IsAlive(_turn)) || (!_isP1Turn && _p2.IsAlive(_turn))));
            OnPlayerStepped(fromCoord, toCoord);

            (bool isOver, bool winner) = IsGameOver();
            if (isOver) OnGameOver(winner);
        }

        private (bool, bool) IsGameOver()
        {
            if (!_p1.IsUnOccupied((_size - 1, 0))) return (true, true);
            if (!_p2.IsUnOccupied((0, _size - 1))) return (true, true);
            return (false, false);
        }
        private List<(int, int)> GetValidCoordinates()
        {
            List<(int, int)> coords = new List<(int, int)>();
            (int x, int y) currentPiece = _isP1Turn ? _p1[_turn] : _p2[_turn];

            for (int i = -1; i < 2; i++)
            {
                for (int j = -1; j < 2; j++)
                {
                    coords.Add((i, j));
                }
            }
            return coords.Where(coord =>
            {
                if ((Math.Abs(coord.Item1) == 1 && coord.Item2 == 1))
                {
                    if (_isP1Turn) return _p1.IsUnOccupied((coord.Item1 + currentPiece.x, coord.Item2 + currentPiece.y));
                    else return _p2.IsUnOccupied((coord.Item1 + currentPiece.x, coord.Item2 + currentPiece.y));
                }
                else
                {
                    return _p1.IsUnOccupied((coord.Item1 + currentPiece.x, coord.Item2 + currentPiece.y)) && _p1.IsUnOccupied((coord.Item1 + currentPiece.x, coord.Item2 + currentPiece.y));
                }
            }).Select(coords => (coords.Item1 + currentPiece.x, coords.Item2 + currentPiece.y)).ToList();
        }

        private void OnPlayerStepped((int, int) fromCoord, (int, int) toCoord)
        {
            PlayerStepped?.Invoke(this, new StepEventArg(fromCoord, toCoord));
        }
        private void OnGameOver(bool winner)
        {
            GameOver?.Invoke(this, winner);
        }
    }
}
