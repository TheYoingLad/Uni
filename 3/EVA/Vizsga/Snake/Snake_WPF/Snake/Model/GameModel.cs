using Snake.Persistence;
using System.Data;

namespace Snake.Model
{
    public class GameModel : IDisposable
    {
        #region Fields and Properties

        // public fields and properties
        public int Resolution { get; set; }
        public virtual List<List<Tile>>? Tiles { get; private set; }
        public int Score { get; private set; } = 0;
        public bool Ingame { get; private set; } = false;
        public Snake? Snake { get { return snake?.Copy(); } }
        public byte TurnMemory { get { return turnMemory; } }
        public virtual List<Coordinate> EggPoses { get => new List<Coordinate>(eggPoses); }
        public bool IsWallsEmpty { get => walls.Count == 0; }

        // private fields
        private Snake? snake;
        private byte turnMemory = 0; // 0: no turn, 1: left, 2: right

        private ITimerService timer = null!;
        private IDataAccess? dataAccess = null;

        private List<Coordinate> walls = new List<Coordinate>();
        private List<Coordinate> eggPoses = new List<Coordinate>();
        private List<Coordinate> posibleEggPositions = new List<Coordinate>();

        private readonly Random rnd = new Random();

        // EventHandlers
        public event EventHandler<EventArgs?>? ResetTiles;
        public event EventHandler<TileArgs>? UpdateTile;
        public event EventHandler<StopGameArgs>? StopEvent;

        #endregion

        #region Constructors and Initialization

        public GameModel(ITimerService timerService)
        {
            ReloadModel(null, timerService, true); // nothing to load in -> initialize map empty
        }

        public GameModel(IDataAccess? dataAccess = null, ITimerService? timerService = null)
        {
            ReloadModel(dataAccess, timerService, true);
        }

        /// <summary>
        /// Loads the map in from ROM if IDataAccess is provided
        /// else it initializes the map empty
        /// </summary>
        /// <param name="clear">If true it generates only Empty Tiles (at Game-start)</param>
        public void ReloadModel(IDataAccess? _dataAccess = null, ITimerService? _timerService = null, bool clear = false)
        {
            // timer
            if (_timerService == null) timer = new TimerService(new TimerCallback(TimerTick!));
            else
            {
                timer = _timerService;
                timer.Tick += TimerTick!;
            }

            // dataAccess
            if (_dataAccess != null) dataAccess = _dataAccess;
            if (dataAccess == null)
            {
                Resolution = 15;
                walls = new List<Coordinate>();
            }
            else (Resolution, walls) = this.dataAccess!.Load();

            // reset game
            ResetGame(clear);
        }

        /// <summary>
        /// Resets all variables to start a new game and 
        /// rebuilds the map in model and in view 
        /// (the view is only rebuilt if resolution has changed else it is just recolored)
        /// </summary>
        /// <param name="clear">If true it generates only Empty Tiles (at Game-start)</param>
        public void ResetGame(bool clear = false)
        {
            // Reset variables to default
            Score = 0;
            turnMemory = 0;
            eggPoses = new List<Coordinate>();
            posibleEggPositions = new List<Coordinate>();
            Tiles = new List<List<Tile>>();
            snake = new Snake(Resolution);

            // Posible Egg Positions
            for (int x = 0; x < Resolution; x++)
            {
                for (int y = 0; y < Resolution; y++)
                {
                    Coordinate coord = new Coordinate(x, y);

                    if (!snake.SnakeBody.Contains(coord) && !walls.Contains(coord)) 
                        posibleEggPositions.Add(coord);
                }
            }

            // Rebuild the Map
            FullGridReset(clear);
        }

        #endregion

        #region Timer

        /// <summary>
        /// If the newGame parameter is set to true it resets the game and start a new one but
        /// if the parameter is set to false it tries to resume a game that has been paused
        /// </summary>
        /// <param name="newGame">Start new game or resume existing one</param>
        public void StartGame(bool newGame = true)
        {
            if (newGame) ResetGame();

            Ingame = true;

            // Start the Timer
            timer.Start();
        }

        /// <summary>
        /// Stop the game (timer): 
        /// if the gameOver parameter is set to true the game ends permanently but
        /// if the parameter is set to false the game is just paused
        /// </summary>
        /// <param name="gameOver">End game permanently or just pause it</param>
        public void StopGame(bool gameOver = false)
        {
            // Stop the Timer
            timer.Stop();

            // Acknowlage if Game Over
            Ingame = !gameOver;

            // Update UI accordingly (for example show Menu UI)
            InvokeStopEvent();
        }

        private void TimerTick(object timerState)
        {
            if (snake == null || Tiles == null) return;

            // Turn the snake according to the latest imput
            if (turnMemory == 1) snake.TurnLeft();
            else if (turnMemory == 2) snake.TurnRight();
            turnMemory = 0;

            // Save Previous Head Position
            Coordinate prevHead = snake.Head;

            // Step forward the snake and update posible egg positions
            (Coordinate coveredTile, Coordinate? uncoveredTile) = snake.StepForward(EggPoses);
            if (uncoveredTile != null) posibleEggPositions.Add(uncoveredTile);
            posibleEggPositions.Remove(coveredTile);

            // Game Over (if snake bites the dust like itself or the wall hehe)
            if (snake.SnakeBody.Take(snake.SnakeBody.Count - 1).Contains(coveredTile) ||
                 walls.Contains(coveredTile))
            {
                StopGame(gameOver: true);
                return;
            }

            // Eating an Egg
            if (Tiles[coveredTile.X][coveredTile.Y].type == TileType.Egg)
            {
                Score++;
                eggPoses.Remove(coveredTile);
                GenerateEgg();
            }

            // Update the Model of the Map
            Tiles[prevHead.X][prevHead.Y].type = TileType.Snake;
            Tiles[coveredTile.X][coveredTile.Y].type = TileType.SnakeHead;
            if (uncoveredTile != null) Tiles[uncoveredTile.X][uncoveredTile.Y].type = TileType.Empty;

            // Update View
            UpdateSingleTile(prevHead);
            UpdateSingleTile(coveredTile);
            if (uncoveredTile != null) UpdateSingleTile(uncoveredTile);
        }

        #endregion

        #region Misc Public and Private Methods

        /// <summary>
        /// Rebuilds the map in model and in view 
        /// (the view is only rebuilt if resolution has changed else it is just recolored)
        /// </summary>
        /// <param name="clear">If true it generates only Empty Tiles (at Game-start)</param>
        /// <exception cref="NoNullAllowedException"></exception>
        private void FullGridReset(bool clear = false)
        {
            if (Tiles == null) throw new NoNullAllowedException("Tiles object was null!");
            if (snake == null) throw new NoNullAllowedException("Snake object was null!");

            if (!clear)
            {
                // Generate Model side map according to snake and wall data
                Tiles.Clear();
                for (int x = 0; x < Resolution; x++)
                {
                    Tiles.Add(new List<Tile>());
                    for (int y = 0; y < Resolution; y++)
                    {
                        Coordinate coord = new Coordinate(x, y);

                        // So the sanke overwrites walls if neccessary!
                        if (snake.SnakeBody.Contains(coord))
                        {
                            // if it is the head
                            if (coord == snake.Head) Tiles[x].Add(new Tile(TileType.SnakeHead));
                            else Tiles[x].Add(new Tile(TileType.Snake));
                        }
                        else if (walls.Contains(coord))
                        {
                            Tiles[x].Add(new Tile(TileType.Wall));
                        }
                        else
                        {
                            Tiles[x].Add(new Tile(TileType.Empty));
                        }
                    }
                }

                // Generate a starting egg
                GenerateEgg();
            }
            else
            {
                // Only create Empty Tiles (at Game-start)
                Tiles.Clear();
                for (int x = 0; x < Resolution; x++)
                {
                    Tiles.Add(new List<Tile>());
                    for (int y = 0; y < Resolution; y++)
                    {
                        Tiles[x].Add(new Tile(TileType.Empty));
                    }
                }
            }

            // Ask View to Update Map
            InvokeTilesUpdate();
        }

        /// <summary>
        /// Resample tile at given position from model and update its color approprietly
        /// </summary>
        /// <param name="pos">Update at this position</param>
        private void UpdateSingleTile(Coordinate pos)
        {
            InvokeTileUpdate(pos);
        }

        /// <summary>
        /// Generate an egg randomly on the map and display it
        /// </summary>
        private void GenerateEgg()
        {
            // Guard Conditions
            if (snake == null) return;
            if (walls.Count + snake.SnakeBody.Count >= Resolution * Resolution
                || posibleEggPositions.Count == 0) // map is filled up
            {
                StopGame(gameOver: true);
                return;
            }

            // Choose a random egg position and mark its position non-choosable in the future
            Coordinate eggPos = posibleEggPositions[rnd.Next(posibleEggPositions.Count)];
            posibleEggPositions.Remove(eggPos);

            // Refresh eggs on map (model and view)
            eggPoses.Add(eggPos);
            Tiles![eggPos.X][eggPos.Y].type = TileType.Egg;
            InvokeTileUpdate(eggPos);
        }

        /// <summary>
        /// Turn the snake left relative to its facing but not immediately.
        /// Store this info until timer tick.
        /// </summary>
        public void TurnLeft()
        {
            turnMemory = 1;
        }

        /// <summary>
        /// Turn the snake rigth relative to its facing but not immediately.
        /// Store this info until timer tick.
        /// </summary>
        public void TurnRight()
        {
            turnMemory = 2;
        }

        #endregion

        #region Invoke Events

        private void InvokeTilesUpdate()
        {
            ResetTiles?.Invoke(this, null);
        }

        private void InvokeTileUpdate(Coordinate pos)
        {
            UpdateTile?.Invoke(this, new TileArgs(pos));
        }

        private void InvokeStopEvent()
        {
            StopEvent?.Invoke(this, new StopGameArgs(this.Ingame));
        }

        #endregion

        #region IDisposable

        /// <summary>
        /// Needed to implement IDisposable in order to dispose Timer safely
        /// </summary>
        public void Dispose()
        {
            if (timer is TimerService TSTimer) TSTimer.Dispose();
        }

        #endregion
    }
}
