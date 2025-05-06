using Minefield.Model;
using Minefield.Persistence;
using Moq;
using System.Timers;

namespace Minefield.Test
{
    [TestClass]
    public class MinefieldUnitTests : IDisposable
    {
        public void Dispose()
        { 
            _testModel.Dispose();
        }

        private MinefieldGameModel _testModel = null!;
        Entity?[,] _testTable = null!;
        private Mock<IMinefieldFileAccess> _mockFileManager = null!;
        private Mock<IMinefieldRandom> _mockRandom = null!;
        private Mock<IMinefieldTimer> _mockTimer = null!;

        [TestInitialize]
        public void Init()
        {
            _testTable = new Entity?[16, 16];
            Player testPlayer = new Player(3, 3);
            _testTable[3, 3] = testPlayer;
            _testTable[3, 4] = new MediumMine(8, 0);
            _testTable[3, 5] = new EasyMine(15, 14);
            _testTable[3, 6] = new HardMine(4, 0);

            _mockFileManager = new Mock<IMinefieldFileAccess>();
            _mockFileManager.Setup(fm => fm.LoadFileAsync(It.IsAny<Stream>())).Returns(() => Task.FromResult((_testTable, testPlayer, new TimeSpan(0,0,0,4,500))));
            
            _mockRandom = new Mock<IMinefieldRandom>();
            _mockRandom.Setup(rnd => rnd.Next(It.IsAny<int>(), It.IsAny<int>())).Returns((int lb, int ub) => (lb + ub) / 2);
            
            _mockTimer = new Mock<IMinefieldTimer>();
            _testModel = new MinefieldGameModel(_mockFileManager.Object, _mockTimer.Object, _mockRandom.Object);
        }

        [TestMethod]
        public void MinefieldModelStartNewGameTest()
        {
            _testModel.StartNewGame();
            Assert.IsTrue(_testModel.IsGameRunning);
            Assert.AreEqual(0, _testModel.GameTime.TotalMilliseconds);
            Assert.IsInstanceOfType(_testModel[_testModel._gridRows / 2, _testModel._gridColumns / 2], typeof(Player));
        }

        [TestMethod]
        public void MinefieldTestPausing()
        {
            _testModel.StartNewGame();
            Assert.IsTrue(!_testModel.IsGamePaused);
            _testModel.TogglePaused();
            Assert.IsTrue(_testModel.IsGamePaused);
            _testModel.TogglePaused();
            Assert.IsTrue(!_testModel.IsGamePaused);
        }

        [TestMethod]
        [DataRow(Direction.Up, -1, 0)]
        [DataRow(Direction.Down, 1, 0)]
        [DataRow(Direction.Left, 0, -1)]
        [DataRow(Direction.Right, 0, 1)]
        public void MinefieldModelMovementTest(Direction dir, int xDif, int yDif)
        {
            _testModel.StartNewGame();
            int x, y;
            (x, y) = _testModel.PlayerCoords;
            _testModel.MovePlayer(dir);
            (int, int) res = _testModel.PlayerCoords;
            Assert.AreEqual((x + xDif, y + yDif), res);
        }

        [TestMethod]
        [DataRow(Direction.Up)]
        [DataRow(Direction.Down)]
        [DataRow(Direction.Left)]
        [DataRow(Direction.Right)]
        public void MinefieldPausedMovement(Direction dir)
        {
            _testModel.StartNewGame();
            _testModel.TogglePaused();
            (int, int) playerLocation = _testModel.PlayerCoords;
            _testModel.MovePlayer(dir);
            Assert.AreEqual(playerLocation, _testModel.PlayerCoords);
        }

        [TestMethod]
        [DataRow(Direction.Up)]
        [DataRow(Direction.Down)]
        [DataRow(Direction.Left)]
        [DataRow(Direction.Right)]
        public void MinefieldMovementBoundsTest(Direction dir)
        {
            _testModel.StartNewGame();
            int movesToBorder;
            switch (dir)
            {
                case Direction.Up:
                    movesToBorder = _testModel.PlayerCoords.Item1 - 1;
                    break;
                case Direction.Down:
                    movesToBorder = _testModel._gridRows - _testModel.PlayerCoords.Item1 - 1;
                    break;
                case Direction.Left:
                    movesToBorder = _testModel.PlayerCoords.Item2;
                    break;
                case Direction.Right:
                    movesToBorder = _testModel._gridColumns - _testModel.PlayerCoords.Item2 - 1;
                    break;
                default:
                    Assert.Fail();
                    return;
            }
            for (int i = 0; i < movesToBorder; i++)
            {
                _testModel.MovePlayer(dir);
            }
            (int, int) playerLocation = _testModel.PlayerCoords;
            _testModel.MovePlayer(dir);
            Assert.AreEqual(playerLocation, _testModel.PlayerCoords);
        }
       
        [TestMethod]
        public void MinefieldTimerTickTimeAdvanceTest()
        {
            _testModel.StartNewGame();
            for (int i = 0; i < 10; i++)
            {
                _mockTimer.Raise(t => t.Elapsed += null, (EventArgs.Empty as ElapsedEventArgs)!);
            }
            Assert.AreEqual(1000, _testModel.GameTime.TotalMilliseconds);   
        }

        [TestMethod]
        public async Task MinefieldMinesDescendingTest()
        {
            await _testModel.LoadGameAsync(Stream.Null);
            _testModel.TogglePaused();
            _mockTimer.Raise(t => t.Elapsed += null, (EventArgs.Empty as ElapsedEventArgs)!);

            Assert.IsInstanceOfType(_testModel[3, 4], typeof(MediumMine));
            Assert.IsNull(_testModel[4, 4]);
            Assert.IsNull(_testModel[3, 5]);
            Assert.IsInstanceOfType(_testModel[4, 5], typeof(EasyMine));
            Assert.IsInstanceOfType(_testModel[3, 6], typeof(HardMine));
            Assert.IsNull(_testModel[4, 6]);
        }

        [TestMethod]
        public async Task MinefieldGameOverMovementTest()
        {
            bool gameOverEventRaised = false;
            _testModel.GameOver += (Object? _, MinefieldEventArgs _) => { gameOverEventRaised = true; };
            await _testModel.LoadGameAsync(Stream.Null);
            _testModel.TogglePaused();
            Assert.IsTrue(_testModel.IsGameRunning);
            Assert.IsTrue(!gameOverEventRaised);
            _testModel.MovePlayer(Direction.Right);
            Assert.IsTrue(!_testModel.IsGameRunning);
            Assert.IsTrue(gameOverEventRaised);
        }

        [TestMethod]
        public async Task MinefieldGameOverMineDescendingOnPlayerTest()
        {
            bool gameOverEventRaised = false;
            _testModel.GameOver += (Object? _, MinefieldEventArgs _) => { gameOverEventRaised = true; };
            await _testModel.LoadGameAsync(Stream.Null);
            _testModel.TogglePaused();
            _testModel.MovePlayer(Direction.Down);
            _testModel.MovePlayer(Direction.Right);
            _testModel.MovePlayer(Direction.Right);
            Assert.IsTrue(_testModel.IsGameRunning);
            Assert.IsTrue(!gameOverEventRaised);
            _mockTimer.Raise(t => t.Elapsed += null, (EventArgs.Empty as ElapsedEventArgs)!);
            Assert.IsTrue(!_testModel.IsGameRunning);
            Assert.IsTrue(gameOverEventRaised);
        }

        [TestMethod]
        public void MinefieldMineSpawningIncreasingTest()
        {
            _testModel.StartNewGame();
            for (int i = 0; i < 84; i++)
            {
                _mockTimer.Raise(t => t.Elapsed += null, (EventArgs.Empty as ElapsedEventArgs)!);
                for (int j = 0; j < _testModel._gridColumns; j++)
                {
                    Assert.IsNull(_testModel[0, j]);
                }
            }

            _mockTimer.Raise(t => t.Elapsed += null, (EventArgs.Empty as ElapsedEventArgs)!);
            int spawnMineCount = 0;
            for (int j = 0; j < _testModel._gridColumns; j++)
            {
                if (_testModel[0, j] is Mine) spawnMineCount++;
            }
            Assert.AreEqual(1, spawnMineCount);
        }

        [TestMethod]
        public async Task MinefieldSaveGameCalledTest()
        {
            _testModel.StartNewGame();
            await _testModel.SaveGameAsync(Stream.Null);
            _mockFileManager.Verify(fm => fm.SaveFileAsync(It.IsAny<Stream>(), It.IsAny<TimeSpan>(), It.IsAny<Entity[,]>()), Times.Once());
        }

        [TestMethod]
        public async Task MinefieldLoadGameCalledTest()
        {
            _testModel.StartNewGame();
            await _testModel.LoadGameAsync(Stream.Null);
            _mockFileManager.Verify(fm => fm.LoadFileAsync(It.IsAny<Stream>()), Times.Once());
        }
        [TestMethod]
        [ExpectedException(typeof(IOException))]
        public async Task MinefieldSaveGameFailedTest()
        {
            _mockFileManager.Setup(fm => fm.SaveFileAsync(It.IsAny<Stream>(), It.IsAny<TimeSpan>(), It.IsAny<Entity[,]>())).Callback(() => throw new IOException());
            _testModel.StartNewGame();
            await _testModel.SaveGameAsync(Stream.Null);
        }

        [TestMethod]
        [ExpectedException(typeof(IOException))]
        public async Task MinefieldLoadGameFailedTest()
        {
            _mockFileManager.Setup(fm => fm.LoadFileAsync(It.IsAny<Stream>())).Returns(() => throw new IOException());
            await _testModel.LoadGameAsync(Stream.Null);
        }

        [TestMethod]
        public async Task MinefieldLoadGameTest()
        {
            await _testModel.LoadGameAsync(Stream.Null);
            Assert.IsInstanceOfType(_testModel[3, 3], typeof(Player));
            Assert.IsInstanceOfType(_testModel[3, 4], typeof(MediumMine));
            Assert.IsInstanceOfType(_testModel[3, 5], typeof(EasyMine));
            Assert.IsInstanceOfType(_testModel[3, 6], typeof(HardMine));
            for (int i = 0; i < _testModel._gridRows; i++)
            {
                for (int j = 0; j < _testModel._gridColumns; j++)
                {
                    if (_testTable[i, j] is null)
                        Assert.IsNull(_testModel[i, j]);
                    else
                        Assert.IsNotNull(_testModel[i, j]);
                }
            }
            Assert.AreEqual(4500, _testModel.GameTime.TotalMilliseconds);
        }
    }
}