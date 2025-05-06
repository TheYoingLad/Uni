using Minefield.Persistence;
using System.Timers;

namespace Minefield.Model
{
    /// <summary>
    /// Irány felsorolás típusa
    /// </summary>
    public enum Direction { Up, Down, Left, Right }

    /// <summary>
    /// Aknamező játék modell típusa
    /// </summary>
    public class MinefieldGameModel : IDisposable
    {
        #region Grid dimensions

        public readonly int _gridRows = 16;
        public readonly int _gridColumns = 16;

        #endregion

        #region Fields

        private IMinefieldFileAccess _fileAccess;   // adatelérés
        private IMinefieldTimer _gameTimer;         // időzítő
        private IMinefieldRandom _random;           // véletlenszám-generátor 
        private Entity?[,] _entities;               // játéktér
        private TimeSpan _gameTime;                 // eltelt játékidő
        private bool _gamePaused;                   // szünetel-e a játék
        private bool _gameRunning;                  // fut-e a játék
        private Player _player;                     // játékos karakter

        #endregion

        #region Properties and Indexer

        /// <summary>
        /// Játék szünetelésének lekérdezése
        /// </summary>
        public bool IsGamePaused { get { return _gamePaused; } }
        
        /// <summary>
        /// Játék futásának lekérdezése
        /// </summary>
        public bool IsGameRunning { get { return _gameRunning; } }

        /// <summary>
        /// Játékos koordinátáinak lekérdezése
        /// </summary>
        public (int, int) PlayerCoords => (_player.X, _player.Y);

        /// <summary>
        /// Játék idejének lekérdezése
        /// </summary>
        public TimeSpan GameTime => _gameTime;

        /// <summary>
        /// Játéktér egy cellája tartalmának lekérdezése
        /// </summary>
        /// <param name="x">X-koordináta (sor)</param>
        /// <param name="y">Y-koordináta (oszlop)</param>
        /// <returns>Cellában lévő Entity objektum, vagy null, ha a cella üres</returns>
        public Entity? this[int x, int y] => _entities[x, y];

        #endregion

        #region Events

        /// <summary>
        /// Idő haladásának az eseménye
        /// </summary>
        public event EventHandler<MinefieldEventArgs>? TimeAdvanced;

        /// <summary>
        /// Játék végének az eseménye
        /// </summary>
        public event EventHandler<MinefieldEventArgs>? GameOver;
        
        /// <summary>
        /// Adott cella tartalmának megváltozásának eseménye
        /// </summary>
        public event EventHandler<MinefieldCellUpdateEventArgs>? CellUpdate;
        
        #endregion

        #region Constructor

        /// <summary>
        /// Játékmodellt példányosító konstruktor
        /// </summary>
        /// <param name="fileAccess">Adatelérést biztosító fájlkezelő</param>
        public MinefieldGameModel(IMinefieldFileAccess fileAccess, IMinefieldTimer timer, IMinefieldRandom random)
        {
            _fileAccess = fileAccess;
            _random = random;
            _entities = new Entity[_gridRows, _gridColumns];
            _gameTime = new TimeSpan(0, 0, 0);

            _gameTimer = timer;
            _gameTimer.Elapsed += TimeElapsed;

            _gamePaused = true;

            _player = new Player(_gridRows/2, _gridColumns/2);
            _entities[_gridRows / 2, _gridColumns / 2] = _player;
        }
        /// <summary>
        /// Játékmodellt példányosító konstruktor
        /// </summary>
        /// <param name="fileAccess">Adatelérést biztosító fájlkezelő</param>
        public MinefieldGameModel(IMinefieldFileAccess fileAccess) : this(fileAccess, new MinefieldTimer(), new MinefieldRandom()) { }

        #endregion

        #region Public methods

        /// <summary>
        /// Új játék előkészítése és indítása
        /// </summary>
        public void StartNewGame()
        {
            Pause();
            _gameTime = _gameTime.Subtract(_gameTime);
            _entities = new Entity[_gridRows, _gridColumns];

            _player = new Player(_gridRows / 2, _gridColumns / 2);
            _entities[_gridRows / 2, _gridColumns / 2] = _player;

            _gameRunning = true;

            for (int x = 0; x < _gridRows; x++)
                for (int y = 0; y < _gridColumns; y++)
                    OnCellChanged(x, y);
            OnTimeElapsed();
            Unpause();
        }

        /// <summary>
        /// Játékos karakter mozgatása adott irányba
        /// </summary>
        /// <param name="direction">Mozgás iránya</param>
        public void MovePlayer(Direction direction)
        {
            if (!_gamePaused)
            {
                int x = _player.X;
                int y = _player.Y;
                switch (direction)
                {
                    case Direction.Up:
                        if (x > 1)
                        {
                            --x;
                        }
                        break;
                    case Direction.Down:
                        if (x < _gridRows - 1)
                        {
                            ++x;
                        }
                        break;
                    case Direction.Left:
                        if (y > 0)
                        {
                            --y;
                        }
                        break;
                    case Direction.Right:
                        if (y < _gridColumns - 1)
                        {
                            ++y;
                        }
                        break;
                }
                if (x != _player.X || y != _player.Y)
                {
                    _entities[_player.X, _player.Y] = null;
                    OnCellChanged(_player.X, _player.Y);
                    _player.X = x;
                    _player.Y = y;
                    if (_entities[x, y] is Mine)
                    {
                        Collision();
                    }
                    else
                    {
                        _entities[x, y] = _player;
                        OnCellChanged(x, y);
                    }
                }
            }
        }

        /// <summary>
        /// Játék szüneteltetésének változtatása
        /// </summary>
        public void TogglePaused()
        {
            if (_gameRunning)
            {
                if (_gamePaused)
                {
                    Unpause();
                }
                else
                {
                    Pause();
                }
            }
        }

        /// <summary>
        /// Játékállás mentése
        /// </summary>
        /// <param name="path">Mentés fájl elérési útvonala</param>
        public async Task SaveGameAsync(string path) => await SaveGameAsync(File.OpenWrite(path));

        /// <summary>
        /// Játékállás mentése
        /// </summary>
        /// <param name="stream">Adatfolyam</param>
        public async Task SaveGameAsync(Stream stream)
        {
            if (IsGameRunning)
                await _fileAccess.SaveFileAsync(stream, _gameTime, _entities);
        }


        /// <summary>
        /// Játékállás betöltése
        /// </summary>
        /// <param name="path">Betöltendő fájl elérési útvonala</param>
        public async Task LoadGameAsync(string path)
        {
            await LoadGameAsync(File.OpenRead(path));
        }

        /// <summary>
        /// Játékállás betöltése
        /// </summary>
        /// <param name="stream">Adatfolyam</param>
        public async Task LoadGameAsync(Stream stream)
        {
            (_entities, _player, _gameTime) = await _fileAccess.LoadFileAsync(stream);
            _gameRunning = true;
            for (int x = 0; x < _gridRows; x++)
                for (int y = 0; y < _gridColumns; y++)
                    OnCellChanged(x, y);
            OnTimeElapsed();
        }

        public void Dispose()
        {
            _gameTimer.Dispose();
        }

        #endregion

        #region Private game methods

        /// <summary>
        /// Játák szüneteltetése
        /// </summary>
        private void Pause()
        {
            _gameTimer.Stop();
            _gamePaused = true;
        }

        /// <summary>
        /// Játék szüneteltetésének feloldása
        /// </summary>
        private void Unpause()
        {
            _gameTimer.Start();
            _gamePaused = false;
        }

        /// <summary>
        /// Játéktér aknáinak süllyesztése
        /// </summary>
        /// <returns>Igaz ha egy akna ütközött a játékos karakterrel</returns>
        private bool MinesDescend()
        {
            bool result = false;
            bool sink;
            int i;
            for (int j = 0; j < _gridColumns; j++)
            {
                i = 0;
                while (i < _gridRows && _entities[i, j] is not Mine)
                {
                    ++i;
                }
                if (i < _gridRows)
                {
                    sink = ((Mine)_entities[i, j]!).Descend();
                    if (sink)
                    {
                        if (i < _gridRows - 1)
                        {
                            if (!result && _entities[i + 1, j] is Player)
                            {
                                result = true;
                            }
                            else
                            {
                                _entities[i + 1, j] = _entities[i, j];
                                OnCellChanged(i + 1, j);
                            }
                        }
                        _entities[i, j] = null;
                        OnCellChanged(i, j);
                    }
                }
            }
            return result;
        }

        /// <summary>
        /// Játékos aknával ütközésének kezelése és játék végének előkészítése
        /// </summary>
        private void Collision()
        {
            _gameTimer.Stop();
            Pause();
            _gameRunning = false;
            _entities[_player.X, _player.Y] = null;
            OnCellChanged(_player.X, _player.Y);
            OnGameOver();
        }

        /// <summary>
        /// Időtől és elérhető oszlopoktól függően véletlen eséllyel vagy egy új aknát helyez a legfelső sorba, vagy nem módosítja a játékteret
        /// </summary>
        private void MaybeSpawnMine()
        {
            List<int> clearCols = new List<int>();
            for (int j = 0; j < _gridColumns; j++)
            {
                int i = 0;
                while (i < _gridRows && _entities[i, j] is not Mine)
                {
                    ++i;
                }
                if (i == _gridRows)
                {
                    clearCols.Add(j);
                }
            }
            if (clearCols.Any())
            {
                int mineCol = clearCols[_random.Next(0, clearCols.Count)];
                int modifier = (int)_gameTime.TotalMilliseconds/100;
                int f = (int)Math.Sqrt(10 * modifier) + 10;
                int g = (int)Math.Sqrt(modifier) + 10;
                int rnd = _random.Next(1, f);
                if (rnd > g && rnd % 10 == 0)
                {
                    switch (rnd % 3)
                    {
                        case 0:
                            _entities[0, mineCol] = new EasyMine();
                            break;
                        case 1:
                            _entities[0, mineCol] = new MediumMine();
                            break;
                        default:
                            _entities[0, mineCol] = new HardMine();
                            break;
                    }
                    OnCellChanged(0, mineCol);
                }
            }
        }


        #endregion

        #region Private event handlin and invocation methods

        /// <summary>
        /// Időzítő tick-jének eseménykezelője
        /// </summary>
        private void TimeElapsed(Object? sender, ElapsedEventArgs e)
        {
            if (!_gamePaused)
            {
                _gameTime = _gameTime.Add(new TimeSpan(0, 0, 0, 0, 100));
                bool collision = MinesDescend();
                if (collision)
                {
                    Collision();
                    return;
                }
                MaybeSpawnMine();
                OnTimeElapsed();
            }
        }

        /// <summary>
        /// Idő haldásának eseménykiváltója
        /// </summary>
        private void OnTimeElapsed()
        {
            TimeAdvanced?.Invoke(this, new MinefieldEventArgs(_gameTime));
        }

        /// <summary>
        /// Játék végének eseménykiváltója
        /// </summary>
        private void OnGameOver()
        {
            GameOver?.Invoke(this, new MinefieldEventArgs(_gameTime));
        }

        /// <summary>
        /// Cella változásának eseménykiváltója
        /// </summary>
        /// <param name="x">X-koordináta (sor)</param>
        /// <param name="y">Y-koordináta (oszlop)</param>
        private void OnCellChanged(int x, int y)
        {
            CellUpdate?.Invoke(this, new MinefieldCellUpdateEventArgs(x, y));
        }

        #endregion
        
    }
}
