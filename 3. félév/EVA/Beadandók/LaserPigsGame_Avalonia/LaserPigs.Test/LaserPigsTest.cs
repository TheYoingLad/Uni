using LaserPigs.Model;
using LaserPigs.Persistence;
using LaserPigs.Persistence.Instructions;
using Moq;

namespace LaserPigs.Test
{
    [TestClass]
    public class LaserPigsTest
    {
        #region Fields
        private Mock<IFileManager> _fileManagerMock = null!;
        private Player _mockP1 = null!;
        private Player _mockP2 = null!;
        private LaserPigsModel _model = null!;
        #endregion

        #region Init
        [TestInitialize]
        public void Init()
        {
            _mockP1 = new Player(6, new Coordinate(3, 5), 4, 4, Direction.Up, 5);
            _mockP2 = new Player(6, new Coordinate(2, 0), 4, 2, Direction.Left, 5);

            _fileManagerMock = new Mock<IFileManager>();

            _model = new LaserPigsModel(_fileManagerMock.Object);
        }
        #endregion

        #region New Game
        [TestMethod]
        public void TestNewGame()
        {
            _model.NewGame();

            Assert.AreEqual(6, _model.Size);

            Assert.AreEqual(0, _model.GetInstructionIndex);

            Assert.AreEqual(true, _model.GetIsP1Active);
            Assert.AreEqual(false, _model.GetIsP2Active);

            Assert.AreEqual(3, _model.GetP1MaxHp);
            Assert.AreEqual(3, _model.GetP2MaxHp);

            Assert.AreEqual(3, _model.GetP1Hp);
            Assert.AreEqual(3, _model.GetP2Hp);

            Assert.AreEqual(Player.StartingDirection(true), _model.GetP1Direction);
            Assert.AreEqual(Player.StartingDirection(false), _model.GetP2Direction);

            Assert.AreEqual(Player.StartingCoordinate(true, 6), _model.GetP1Coord);
            Assert.AreEqual(Player.StartingCoordinate(false, 6), _model.GetP2Coord);

            Assert.AreEqual(0, _model.GetP1InstructionCount);
            Assert.AreEqual(0, _model.GetP2InstructionCount);

            Instruction[] instructions = _model.GetP1Instructions;
            for (int i = 0; i < 5; i++)
            {
                Assert.AreEqual(null, instructions[i]);
            }

            instructions = _model.GetP2Instructions;
            for (int i = 0; i < 5; i++)
            {
                Assert.AreEqual(null, instructions[i]);
            }
        }
        #endregion

        #region Load Game
        [TestMethod]
        public async Task TestLoadGameInvalidMapSize()
        {
            _mockP1 = new Player(7, new Coordinate(1, 1), 4, 4, Direction.Up, 5);
            _mockP2 = new Player(7, new Coordinate(1, 1), 4, 2, Direction.Left, 5);

            _fileManagerMock.Setup(mock => mock.LoadAsync(It.IsAny<string>())).Returns(() => Task.FromResult((_mockP1, _mockP2, 0)));
            await Assert.ThrowsExceptionAsync<ArgumentException>(() => _model.LoadGameAsync(It.IsAny<string>()));

            _fileManagerMock.Verify(fm => fm.LoadAsync(It.IsAny<string>()), Times.Once);
        }
        [TestMethod]
        public async Task TestLoadGameSameCoords()
        {
            _mockP1 = new Player(6, new Coordinate(1, 1), 4, 4, Direction.Up, 5);
            _mockP2 = new Player(6, new Coordinate(1, 1), 4, 2, Direction.Left, 5);

            _fileManagerMock.Setup(mock => mock.LoadAsync(It.IsAny<string>())).Returns(() => Task.FromResult((_mockP1, _mockP2, 0)));
            await Assert.ThrowsExceptionAsync<ArgumentException>(() => _model.LoadGameAsync(It.IsAny<string>()));

            _fileManagerMock.Verify(fm => fm.LoadAsync(It.IsAny<string>()), Times.Once);
        }
        [TestMethod]
        public async Task TestLoadGameInvalidP2()
        {
            FillP2();

            _fileManagerMock.Setup(mock => mock.LoadAsync(It.IsAny<string>())).Returns(() => Task.FromResult((_mockP1, _mockP2, 0)));
            await Assert.ThrowsExceptionAsync<ArgumentException>(() => _model.LoadGameAsync(It.IsAny<string>()));

            _fileManagerMock.Verify(fm => fm.LoadAsync(It.IsAny<string>()), Times.Once);
        }
        [TestMethod]
        public async Task TestLoadGameInvalidInstructionIndex()
        {
            _fileManagerMock.Setup(mock => mock.LoadAsync(It.IsAny<string>())).Returns(() => Task.FromResult((_mockP1, _mockP2, 2)));
            await Assert.ThrowsExceptionAsync<ArgumentException>(() => _model.LoadGameAsync(It.IsAny<string>()));

            _fileManagerMock.Setup(mock => mock.LoadAsync(It.IsAny<string>())).Returns(() => Task.FromResult((_mockP1, _mockP2, -1)));
            await Assert.ThrowsExceptionAsync<ArgumentException>(() => _model.LoadGameAsync(It.IsAny<string>()));

            _fileManagerMock.Setup(mock => mock.LoadAsync(It.IsAny<string>())).Returns(() => Task.FromResult((_mockP1, _mockP2, 6)));
            await Assert.ThrowsExceptionAsync<ArgumentException>(() => _model.LoadGameAsync(It.IsAny<string>()));

            _fileManagerMock.Verify(fm => fm.LoadAsync(It.IsAny<string>()), Times.Exactly(3));
        }
        [TestMethod]
        public async Task TestLoadGame()
        {
            bool gameLoadedEventFired = false;
            _model.GameLoaded += (sender, args) => { gameLoadedEventFired = true; };

            MockLoadPhaseFight();
            await _model.LoadGameAsync(It.IsAny<string>());

            Assert.AreEqual(6, _model.Size);

            Assert.AreEqual(4, _model.GetP1MaxHp);
            Assert.AreEqual(4, _model.GetP2MaxHp);

            Assert.AreEqual(4, _model.GetP1Hp);
            Assert.AreEqual(2, _model.GetP2Hp);

            Assert.AreEqual(Direction.Up, _model.GetP1Direction);
            Assert.AreEqual(Direction.Left, _model.GetP2Direction);

            Assert.AreEqual(new Coordinate(3, 5), _model.GetP1Coord);
            Assert.AreEqual(new Coordinate(2, 0), _model.GetP2Coord);

            Assert.AreEqual(5, _model.GetP1InstructionCount);
            Assert.AreEqual(5, _model.GetP2InstructionCount);

            Instruction[] instructions = _model.GetP1Instructions;
            for (int i = 0; i < 5; i++)
            {
                Assert.AreEqual(Direction.Up, (instructions[i] as Step)!.GetDirection);
            }

            instructions = _model.GetP2Instructions;
            for (int i = 0; i < 5; i++)
            {
                Assert.AreEqual(Direction.Up, (instructions[i] as Step)!.GetDirection);
            }

            Assert.AreEqual(2, _model.GetInstructionIndex);

            Assert.AreEqual(false, _model.GetIsP1Active);
            Assert.AreEqual(false, _model.GetIsP2Active);

            Assert.AreEqual(true, gameLoadedEventFired);

            _fileManagerMock.Verify(fm => fm.LoadAsync(It.IsAny<string>()), Times.Once);
        }
        #endregion

        #region Save Game
        [TestMethod]
        public async Task TestSaveGameVerify()
        {
            _fileManagerMock.Setup(mock => mock.LoadAsync(It.IsAny<string>())).Returns(() => Task.FromResult((_mockP1, _mockP2, 0)));
            await _model.LoadGameAsync(It.IsAny<string>());
            await _model.SaveGameAsync("testNew.lspf");

            _fileManagerMock.Verify(fm => fm.SaveAsync("testNew.lspf", _mockP1, _mockP2, 0), Times.Once);
        }
        [TestMethod]
        public async Task TestSaveNewGame()
        {
            IFileManager fileManager = new FileManager();
            _model = new LaserPigsModel(fileManager);

            await _model.SaveGameAsync("testNew.lspf");
            await _model.LoadGameAsync("testNew.lspf");

            Assert.AreEqual(6, _model.Size);

            Assert.AreEqual(0, _model.GetInstructionIndex);

            Assert.AreEqual(true, _model.GetIsP1Active);
            Assert.AreEqual(false, _model.GetIsP2Active);

            Assert.AreEqual(3, _model.GetP1MaxHp);
            Assert.AreEqual(3, _model.GetP2MaxHp);

            Assert.AreEqual(3, _model.GetP1Hp);
            Assert.AreEqual(3, _model.GetP2Hp);

            Assert.AreEqual(Player.StartingDirection(true), _model.GetP1Direction);
            Assert.AreEqual(Player.StartingDirection(false), _model.GetP2Direction);

            Assert.AreEqual(Player.StartingCoordinate(true, 6), _model.GetP1Coord);
            Assert.AreEqual(Player.StartingCoordinate(false, 6), _model.GetP2Coord);

            Assert.AreEqual(0, _model.GetP1InstructionCount);
            Assert.AreEqual(0, _model.GetP2InstructionCount);

            Instruction[] instructions = _model.GetP1Instructions;
            for (int i = 0; i < 5; i++)
            {
                Assert.AreEqual(null, instructions[i]);
            }

            instructions = _model.GetP2Instructions;
            for (int i = 0; i < 5; i++)
            {
                Assert.AreEqual(null, instructions[i]);
            }
        }
        #endregion

        #region Add Instruction
        [TestMethod]
        public async Task TestAddInstructionFull()
        {
            MockLoadPhase1();
            await _model.LoadGameAsync(It.IsAny<string>());
            _model.AddInstruction(new Step(Direction.Up));

            Assert.ThrowsException<InvalidOperationException>(() => _model.AddInstruction(new Shoot()));
        }
        [TestMethod]
        public async Task TestAddInstructionLocked()
        {
            MockLoadPhaseFight();
            await _model.LoadGameAsync(It.IsAny<string>());

            Assert.ThrowsException<InvalidOperationException>(() => _model.AddInstruction(new Shoot()));
        }
        [TestMethod]
        public void TestAddInstruction()
        {
            bool instructionAddedEventFired = false;
            _model.InstructionAdded += (sender, args) => { instructionAddedEventFired = true; };

            _model.AddInstruction(new Step(Direction.Up));

            Assert.AreEqual(1, _model.GetP1InstructionCount);
            Assert.AreEqual(Direction.Up, (_model.GetP1Instructions[0] as Step)!.GetDirection);
            Assert.AreEqual(true, instructionAddedEventFired);
        }
        #endregion

        #region Remove Instruction
        [TestMethod]
        public void TestRemoveInstructionEmpty()
        {
            Assert.ThrowsException<InvalidOperationException>(() => _model.RemoveInstruction());
        }
        [TestMethod]
        public async Task TestRemoveInstructionLocked()
        {
            MockLoadPhaseFight();
            await _model.LoadGameAsync(It.IsAny<string>());

            Assert.ThrowsException<InvalidOperationException>(() => _model.RemoveInstruction());
        }
        [TestMethod]
        public async Task TestRemoveInstruction()
        {
            bool instructionRemovedEventFired = false;
            _model.InstructionRemoved += (sender, args) => { instructionRemovedEventFired = true; };

            MockLoadPhase1();
            await _model.LoadGameAsync(It.IsAny<string>());

            _model.RemoveInstruction();

            Assert.AreEqual(3, _model.GetP1InstructionCount);
            Assert.AreEqual(true, instructionRemovedEventFired);
        }
        #endregion

        #region Next
        [TestMethod]
        public void TestNextWrongPhase()
        {
            Assert.ThrowsException<InvalidOperationException>(() => _model.Next());
        }
        [TestMethod]
        public void TestNextAllDone()
        {
            FillP1();
            FillP2();
            _fileManagerMock.Setup(mock => mock.LoadAsync(It.IsAny<string>())).Returns(() => Task.FromResult((_mockP1, _mockP2, 5)));

            Assert.ThrowsException<InvalidOperationException>(() => _model.Next());
        }
        [TestMethod]
        public async Task TestNextStep()
        {
            bool playerSteppedEventFired = false;
            _model.PlayerStepped += (sender, args) => { playerSteppedEventFired = true; };
            bool instructionProcessedEventFired = false;
            _model.InstructionProcessed += (sender, args) => { instructionProcessedEventFired = true; };

            _mockP1 = new Player(6, new Coordinate(5, 3), 4, 4, Direction.Up, 1);
            _mockP2 = new Player(6, new Coordinate(0, 2), 4, 2, Direction.Left, 1);

            _mockP1.AddInstruction(new Step(Direction.Up));
            _mockP1.Locked = true;
            _mockP2.AddInstruction(new Step(Direction.Up));
            _mockP2.Locked = true;

            _fileManagerMock.Setup(mock => mock.LoadAsync(It.IsAny<string>())).Returns(() => Task.FromResult((_mockP1, _mockP2, 0)));
            await _model.LoadGameAsync(It.IsAny<string>());

            _model.Next();

            Assert.AreEqual(new Coordinate(4, 3), _model.GetP1Coord);
            Assert.AreEqual(new Coordinate(0, 2), _model.GetP2Coord);

            Assert.AreEqual(1, _model.GetInstructionIndex);

            Assert.AreEqual(true, playerSteppedEventFired);
            Assert.AreEqual(true, instructionProcessedEventFired);
        }
        [TestMethod]
        public async Task TestNextTurn()
        {
            bool playerTurnedEventFired = false;
            _model.PlayerTurned += (sender, args) => { playerTurnedEventFired = true; };
            bool instructionProcessedEventFired = false;
            _model.InstructionProcessed += (sender, args) => { instructionProcessedEventFired = true; };

            _mockP1 = new Player(6, new Coordinate(5, 3), 4, 4, Direction.Up, 1);
            _mockP2 = new Player(6, new Coordinate(0, 2), 4, 2, Direction.Left, 1);

            _mockP1.AddInstruction(new Turn(Rotation.CW));
            _mockP1.Locked = true;
            _mockP2.AddInstruction(new Turn(Rotation.CCW));
            _mockP2.Locked = true;

            _fileManagerMock.Setup(mock => mock.LoadAsync(It.IsAny<string>())).Returns(() => Task.FromResult((_mockP1, _mockP2, 0)));
            await _model.LoadGameAsync(It.IsAny<string>());

            _model.Next();

            Assert.AreEqual(Direction.Right, _model.GetP1Direction);
            Assert.AreEqual(Direction.Down, _model.GetP2Direction);

            Assert.AreEqual(1, _model.GetInstructionIndex);

            Assert.AreEqual(true, playerTurnedEventFired);
            Assert.AreEqual(true, instructionProcessedEventFired);
        }
        [TestMethod]
        public async Task TestNextAttackVictory()
        {
            bool playerDamagedEventFired = false;
            _model.PlayerDamaged += (sender, args) => { playerDamagedEventFired = true; };
            bool gameOverEventFired = false;
            _model.GameOver += (sender, args) => { gameOverEventFired = true; };
            bool instructionProcessedEventFired = false;
            _model.InstructionProcessed += (sender, args) => { instructionProcessedEventFired = true; };

            _mockP1 = new Player(6, new Coordinate(2, 2), 4, 4, Direction.Right, 1);
            _mockP2 = new Player(6, new Coordinate(2, 3), 4, 1, Direction.Up, 1);

            _mockP1.AddInstruction(new Shoot());
            _mockP1.Locked = true;
            _mockP2.AddInstruction(new Punch());
            _mockP2.Locked = true;

            _fileManagerMock.Setup(mock => mock.LoadAsync(It.IsAny<string>())).Returns(() => Task.FromResult((_mockP1, _mockP2, 0)));
            await _model.LoadGameAsync(It.IsAny<string>());

            _model.Next();

            Assert.AreEqual(3, _model.GetP1Hp);
            Assert.AreEqual(0, _model.GetP2Hp);

            Assert.AreEqual(1, _model.GetInstructionIndex);

            Assert.AreEqual(true, playerDamagedEventFired);
            Assert.AreEqual(true, gameOverEventFired);
            Assert.AreEqual(true, instructionProcessedEventFired);
        }
        [TestMethod]
        public async Task TestNextAttackDraw()
        {
            bool playerDamagedEventFired = false;
            _model.PlayerDamaged += (sender, args) => { playerDamagedEventFired = true; };
            bool gameOverEventFired = false;
            _model.GameOver += (sender, args) => { gameOverEventFired = true; };
            bool instructionProcessedEventFired = false;
            _model.InstructionProcessed += (sender, args) => { instructionProcessedEventFired = true; };

            _mockP1 = new Player(6, new Coordinate(2, 2), 4, 1, Direction.Right, 1);
            _mockP2 = new Player(6, new Coordinate(2, 3), 4, 1, Direction.Up, 1);

            _mockP1.AddInstruction(new Punch());
            _mockP1.Locked = true;
            _mockP2.AddInstruction(new Punch());
            _mockP2.Locked = true;

            _fileManagerMock.Setup(mock => mock.LoadAsync(It.IsAny<string>())).Returns(() => Task.FromResult((_mockP1, _mockP2, 0)));
            await _model.LoadGameAsync(It.IsAny<string>());

            _model.Next();

            Assert.AreEqual(0, _model.GetP1Hp);
            Assert.AreEqual(0, _model.GetP2Hp);

            Assert.AreEqual(1, _model.GetInstructionIndex);

            Assert.AreEqual(true, playerDamagedEventFired);
            Assert.AreEqual(true, gameOverEventFired);
            Assert.AreEqual(true, instructionProcessedEventFired);
        }
        #endregion

        #region Confirm
        [TestMethod]
        public async Task TestConfirmP1NotFull()
        {
            MockLoadPhase1();
            await _model.LoadGameAsync(It.IsAny<string>());

            Assert.ThrowsException<InvalidOperationException>(() => _model.Confirm());
        }
        [TestMethod]
        public async Task TestConfirmP1()
        {
            bool phaseChangedEventFired = false;
            _model.PhaseChanged += (sender, args) => { phaseChangedEventFired = true; };

            MockLoadPhase1();
            _mockP1.AddInstruction(new Step(Direction.Up));
            await _model.LoadGameAsync(It.IsAny<string>());

            _model.Confirm();

            Assert.AreEqual(false, _model.GetIsP1Active);
            Assert.AreEqual(true, _model.GetIsP2Active);

            Assert.AreEqual(true, phaseChangedEventFired);
        }
        [TestMethod]
        public async Task TestConfirmP2NotFull()
        {
            MockLoadPhase2();
            await _model.LoadGameAsync(It.IsAny<string>());

            Assert.ThrowsException<InvalidOperationException>(() => _model.Confirm());
        }
        [TestMethod]
        public async Task TestConfirmP2()
        {
            bool phaseChangedEventFired = false;
            _model.PhaseChanged += (sender, args) => { phaseChangedEventFired = true; };

            MockLoadPhase2();
            _mockP2.AddInstruction(new Step(Direction.Up));
            await _model.LoadGameAsync(It.IsAny<string>());

            _model.Confirm();

            Assert.AreEqual(false, _model.GetIsP1Active);
            Assert.AreEqual(false, _model.GetIsP2Active);

            Assert.AreEqual(true, phaseChangedEventFired);
        }
        [TestMethod]
        public async Task TestConfirmFightNotDone()
        {
            MockLoadPhaseFight();
            await _model.LoadGameAsync(It.IsAny<string>());

            Assert.ThrowsException<InvalidOperationException>(() => _model.Confirm());
        }
        [TestMethod]
        public async Task TestConfirm()
        {
            bool phaseChangedEventFired = false;
            _model.PhaseChanged += (sender, args) => { phaseChangedEventFired = true; };

            MockLoadPhaseFight();
            _fileManagerMock.Setup(mock => mock.LoadAsync(It.IsAny<string>())).Returns(() => Task.FromResult((_mockP1, _mockP2, 5)));
            await _model.LoadGameAsync(It.IsAny<string>());

            _model.Confirm();

            Assert.AreEqual(0, _model.GetP1InstructionCount);
            Assert.AreEqual(0, _model.GetP2InstructionCount);

            Assert.AreEqual(true, _model.GetIsP1Active);
            Assert.AreEqual(false, _model.GetIsP2Active);

            Assert.AreEqual(0, _model.GetInstructionIndex);

            Assert.AreEqual(true, phaseChangedEventFired);
        }
        #endregion

        #region Helper Functions
        private void MockLoadPhase1()
        {
            ResetPlayers();

            FillP1();
            _mockP1.RemoveInstruction();

            _fileManagerMock.Setup(mock => mock.LoadAsync(It.IsAny<string>())).Returns(() => Task.FromResult((_mockP1, _mockP2, 0)));
        }
        private void MockLoadPhase2()
        {
            ResetPlayers();

            FillP1();
            _mockP1.Locked = true;

            FillP2();
            _mockP2.RemoveInstruction();

            _fileManagerMock.Setup(mock => mock.LoadAsync(It.IsAny<string>())).Returns(() => Task.FromResult((_mockP1, _mockP2, 0)));
        }
        private void MockLoadPhaseFight()
        {
            ResetPlayers();

            FillP1();
            _mockP1.Locked = true;

            FillP2();
            _mockP2.Locked = true;

            _fileManagerMock.Setup(mock => mock.LoadAsync(It.IsAny<string>())).Returns(() => Task.FromResult((_mockP1, _mockP2, 2)));
        }
        private void ResetPlayers()
        {
            _mockP1 = new Player(6, new Coordinate(3, 5), 4, 4, Direction.Up, 5);
            _mockP2 = new Player(6, new Coordinate(2, 0), 4, 2, Direction.Left, 5);
        }
        private void FillP1()
        {
            _mockP1.AddInstruction(new Step(Direction.Up));
            _mockP1.AddInstruction(new Step(Direction.Up));
            _mockP1.AddInstruction(new Step(Direction.Up));
            _mockP1.AddInstruction(new Step(Direction.Up));
            _mockP1.AddInstruction(new Step(Direction.Up));
        }
        private void FillP2()
        {
            _mockP2.AddInstruction(new Step(Direction.Up));
            _mockP2.AddInstruction(new Step(Direction.Up));
            _mockP2.AddInstruction(new Step(Direction.Up));
            _mockP2.AddInstruction(new Step(Direction.Up));
            _mockP2.AddInstruction(new Step(Direction.Up));
        }
        #endregion
    }
}