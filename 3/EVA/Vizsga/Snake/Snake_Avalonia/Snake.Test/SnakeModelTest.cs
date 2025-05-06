using Moq;
using Snake.Model;
using Snake.Persistence;
using System.Collections.Generic;
using System.Data;
using System.Reflection;

namespace Snake.Tests
{
    #region Helper Class(es)

    internal class TesterTimer : ITimerService
    {
        public event TimerCallback? Tick;

        public void Start()
        {
            return; // do nothing
        }

        public void Stop()
        {
            return; // do nothing
        }

        public void TickManually()
        {
            Tick?.Invoke(null);
        }
    }

    #endregion

    [TestClass]
    public class GameModelTests : IDisposable
    {
        private GameModel game;
        private TesterTimer timer;

        public GameModelTests()
        {
            timer = new TesterTimer();
            game = new GameModel();
        }

        [TestInitialize]
        public async Task Initialize()
        {
            await game.ReloadModel(null, timer, false);
        }

        #region TestMethods

        [TestMethod]
        public void ModelInitializeTest()
        {
            game.ResetGame();

            Assert.AreEqual(0, game.Score);
            Assert.AreEqual(15, game.Resolution);
            Assert.IsNotNull(game.Tiles);
            Assert.IsNotNull(game.Snake);
            Assert.IsFalse(game.Ingame);
        }

        [TestMethod]
        public void TurnLeftTest()
        {
            game.ResetGame();

            game.TurnLeft();
            byte turnMemory = game.TurnMemory;
            Assert.AreEqual(1, turnMemory);

            timer.TickManually();

            Direction? dir = game.Snake?.Direction;
            Assert.AreEqual(Direction.LEFT, dir);
        }

        [TestMethod]
        public void TurnRightTest()
        {
            game.ResetGame();

            game.TurnRight();
            byte turnMemory = game.TurnMemory;
            Assert.AreEqual(2, turnMemory);

            timer.TickManually();

            Direction? dir = game.Snake?.Direction;
            Assert.AreEqual(Direction.RIGHT, dir);
        }

        [TestMethod]
        public async Task IncreaseScoreTest()
        {
            Coordinate eggPos = new Coordinate(7, 4);

            TesterTimer manualTimer = new TesterTimer();
            Mock<GameModel> mockedGame = new Mock<GameModel>() { CallBase = true};
            await mockedGame.Object.ReloadModel(null, manualTimer, false);

            mockedGame.Setup(x => x.EggPoses).Returns(new List<Coordinate> { eggPos });
            mockedGame.Object.Tiles![eggPos.X][eggPos.Y].type = TileType.Egg;

            PrintBody(mockedGame);

            manualTimer.TickManually();

            Console.WriteLine("-----");
            PrintBody(mockedGame);

            Assert.AreEqual(1, mockedGame.Object.Score);

            void PrintBody(Mock<GameModel> mockedGame)
            {
                Queue<Coordinate>? body = mockedGame.Object.Snake?.SnakeBody;
                if (body == null) Assert.Fail("SnakeBody was null!");
                foreach (Coordinate coord in body)
                {
                    Console.WriteLine($"x: {coord.X}; y: {coord.Y}");
                }
            }
        }

        [TestMethod]
        public void SelfCollisionTest()
        {
            game.ResetGame();
            game.StartGame(true);

            for (int i = 0; i < 4; i++)
            {
                game.TurnRight();
                timer.TickManually();
            }

            Assert.IsFalse(game.Ingame);

            game.ResetGame();
        }

        [TestMethod]
        public void VerticalTorusTest() // Wrapping around test
        {
            game.ResetGame();

            for (int i = 0; i < 15; i++)
            {
                timer.TickManually();
            }

            Assert.AreEqual(TileType.SnakeHead, game.Tiles![7][5].type);
        }

        [TestMethod]
        public void HorizontalTorusTest() // Wrapping around test
        {
            game.ResetGame();
            game.TurnRight();

            for (int i = 0; i < 15; i++)
            {
                timer.TickManually();
            }

            Assert.AreEqual(TileType.SnakeHead, game.Tiles![7][5].type);
        }

        [TestMethod]
        public async Task SaturatedMapTest()
        {
            TesterTimer manualTimer = new TesterTimer();
            IDataAccess dataAccess = new SaveFileDataAccess("test.save");
            GameModel test = await GameModel.Create(dataAccess, manualTimer);

            Console.WriteLine(test.Resolution);
            test.StartGame(true);
            Console.WriteLine(test.Resolution);
            Console.WriteLine(test.Ingame);

            manualTimer.TickManually();

            Assert.IsFalse(test.Ingame);
            test.Dispose();
        }

        [TestMethod]
        public async Task WallCollisionTest()
        {
            TesterTimer manualTimer = new TesterTimer();
            IDataAccess dataAccess = new SaveFileDataAccess("test2.save");
            GameModel test = await GameModel.Create(dataAccess, manualTimer);

            test.StartGame(true);
            Console.WriteLine(test.Ingame);

            manualTimer.TickManually();

            Assert.IsFalse(test.Ingame);
            test.Dispose();
        }

        [TestMethod]
        public void WrongFormatTest()
        {
            try
            {
                Task.Run(async () =>
                {
                    IDataAccess dataAccess = new SaveFileDataAccess("test3.save");
                    GameModel test = await GameModel.Create(dataAccess);
                    test?.Dispose();

                    Assert.Fail();
                });
            }
            catch (Exception)
            {
                Assert.IsTrue(true);
            }
        }

        #endregion

        #region Unused Utility

        //private T? GetPrivateFieldValue<T>(object obj, string fieldName)
        //{
        //    var field = typeof(GameModel).GetField(fieldName, BindingFlags.NonPublic | BindingFlags.Instance);
        //    return (T?)field?.GetValue(obj);
        //}

        //private void SetPrivateFieldValue<T>(object obj, string fieldName, T value)
        //{
        //    var field = typeof(GameModel).GetField(fieldName, BindingFlags.NonPublic | BindingFlags.Instance);
        //    if (field != null) field.SetValue(obj, value);
        //    else throw new NoNullAllowedException($"Field cannot be set: {fieldName}");
        //}

        //private void SetPrivatePropertyValue<T>(object obj, string propertyName, T value)
        //{
        //    var property = typeof(GameModel).GetProperty(propertyName);
        //    if (property != null) property.SetValue(obj, value);
        //    else throw new NoNullAllowedException($"Property cannot be set: {propertyName}");
        //}

        //private void InvokePrivateMethod(object obj, string methodName, object?[]? parameters)
        //{
        //    var method = typeof(GameModel).GetMethod(methodName, BindingFlags.NonPublic | BindingFlags.Instance);
        //    if (method != null) method.Invoke(obj, parameters);
        //    else throw new NoNullAllowedException($"Method cannot be called: {methodName}");
        //}

        #endregion

        #region IDisposable stuff

        public void Dispose()
        {
            game.Dispose();
        }

        #endregion
    }
}