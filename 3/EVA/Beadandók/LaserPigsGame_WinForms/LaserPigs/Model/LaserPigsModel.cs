using LaserPigs.Persistence;
using LaserPigs.Persistence.Instructions;

namespace LaserPigs.Model
{
    public enum Phase { P1, P2, Fight }

    public class LaserPigsModel
    {
        #region Fields
        private int _size;
        private Player _p1;
        private Player _p2;
        private int _instructionIndex;
        private IFileManager _fileManager;
        private Phase _phase;
        #endregion

        #region Constructors
        /// <summary>
        /// Initialises the model
        /// </summary>
        /// <param name="fileManager">Laser Pigs file manager</param>
        public LaserPigsModel(IFileManager fileManager)
        {
            _size = 6;
            _p1 = new Player(true, _size);
            _p2 = new Player(false, _size);
            _instructionIndex = 0;
            _fileManager = fileManager;
            _phase = Phase.P1;
        }
        #endregion

        #region Properties
        /// <summary>
        /// Gets or sets the size of the map
        /// </summary>
        public int Size { get { return _size; } set { _size = value; } }
        /// <summary>
        /// Gets the coordinate of player 1
        /// </summary>
        public Coordinate GetP1Coord { get { return _p1.GetCoordinate; } }
        /// <summary>
        /// Gets the coordinate of player 2
        /// </summary>
        public Coordinate GetP2Coord { get { return _p2.GetCoordinate; } }
        /// <summary>
        /// Gets the direction of player 1
        /// </summary>
        public Direction GetP1Direction { get { return _p1.GetDirection; } }
        /// <summary>
        /// Gets the direction of player 2
        /// </summary>
        public Direction GetP2Direction { get { return _p2.GetDirection; } }
        /// <summary>
        /// Gets whether player 1 is active or not
        /// </summary>
        public bool GetIsP1Active { get { return _phase == Phase.P1; } }
        /// <summary>
        /// Gets whether player 2 is active or not
        /// </summary>
        public bool GetIsP2Active { get { return _phase == Phase.P2; } }
        /// <summary>
        /// Gets the length of the instructions array
        /// </summary>
        public int GetMaxInstructions { get { return _p1.GetMaxInstructions; } }
        /// <summary>
        /// Gets the index of the next instruction to be processed
        /// </summary>
        public int GetInstructionIndex { get { return _instructionIndex; } }
        /// <summary>
        /// Gets the max HP of player 1
        /// </summary>
        public int GetP1MaxHp { get { return _p1.GetMaxHp; } }
        /// <summary>
        /// Gets the max HP of player 2
        /// </summary>
        public int GetP2MaxHp { get { return _p2.GetMaxHp; } }
        /// <summary>
        /// Gets the current HP of player 1
        /// </summary>
        public int GetP1Hp { get { return _p1.GetHp; } }
        /// <summary>
        /// Gets the current HP of player 2
        /// </summary>
        public int GetP2Hp { get { return _p2.GetHp; } }
        /// <summary>
        /// Gets a copy of the instructions array of player 1
        /// </summary>
        public Instruction[] GetP1Instructions { get { return _p1.GetInstructions; } }
        /// <summary>
        /// Gets a copy of the instructions array of player 2
        /// </summary>
        public Instruction[] GetP2Instructions { get { return _p2.GetInstructions; } }
        /// <summary>
        /// Gets the current amount of instructions in the instructions array of player 1
        /// </summary>
        public int GetP1InstructionCount { get { return _p1.GetInstructionCount; } }
        /// <summary>
        /// Gets the current amount of instructions in the instructions array of player 2
        /// </summary>
        public int GetP2InstructionCount { get { return _p2.GetInstructionCount; } }
        #endregion

        #region Events
        /// <summary>
        /// Event of a player stepping
        /// </summary>
        public event EventHandler<StepEventArgs>? PlayerStepped;
        /// <summary>
        /// Event of a player turning
        /// </summary>
        public event EventHandler<TurnEventArgs>? PlayerTurned;
        /// <summary>
        /// Event of a player taking damage
        /// </summary>
        public event EventHandler<DamageEventArgs>? PlayerDamaged;
        /// <summary>
        /// Event of the game ending
        /// </summary>
        public event EventHandler<GameOverEventArgs>? GameOver;
        /// <summary>
        /// Event of an instruction getting processed
        /// </summary>
        public event EventHandler<InstructionEventArgs>? InstructionProcessed;
        /// <summary>
        /// Event of an instruction getting added
        /// </summary>
        public event EventHandler<InstructionEventArgs>? InstructionAdded;
        /// <summary>
        /// Event of an instruction getting removed
        /// </summary>
        public event EventHandler<EventArgs>? InstructionRemoved;
        /// <summary>
        /// Event of the game phase changing
        /// </summary>
        public event EventHandler<PhaseEventArgs>? PhaseChanged;
        /// <summary>
        /// Event of the game loading from a file 
        /// </summary>
        public event EventHandler<PhaseEventArgs>? GameLoaded;
        #endregion

        #region Public game methods
        /// <summary>
        /// Stars a new game by resetting the model
        /// </summary>
        public void NewGame()
        {
            _p1 = new Player(true, _size);
            _p2 = new Player(false, _size);
            _instructionIndex = 0;
            _phase = Phase.P1;
        }
        /// <summary>
        /// Loads a game from a file
        /// </summary>
        /// <param name="path">Path of the file</param>
        /// <exception cref="ArgumentException"></exception>
        public void LoadGame(string path)
        {
            (Player p1, Player p2, int instructionIndex) = _fileManager.Load(path);

            if (p1.GetSize != 4 && p1.GetSize != 6 && p1.GetSize != 8) throw new ArgumentException("The map must be of size 4x4 or 6x6 or 8x8", nameof(p1.GetSize));
            if (p1.GetCoordinate == p2.GetCoordinate) throw new ArgumentException("The players cannot be in the same position", nameof(p2));
            if (!p1.Locked && p2.GetInstructionCount > 0) throw new ArgumentException("The instructions array of Player 1 in not locked and the instructions array of Player 2 is not empty", nameof(p2));
            if (!p2.Locked && instructionIndex != 0) throw new ArgumentException("The fight phase has not started", nameof(instructionIndex));
            if (instructionIndex < 0) throw new ArgumentException("The instruction index is less than 0", nameof(instructionIndex));
            if (instructionIndex > p1.GetMaxInstructions) throw new ArgumentException("The instruction index is more than the size of the instructions array", nameof(instructionIndex));

            _size = p1.GetSize;
            _p1 = p1;
            _p2 = p2;
            _instructionIndex = instructionIndex;

            if (p2.Locked) _phase = Phase.Fight;
            else if (p1.Locked) _phase = Phase.P2;
            else _phase = Phase.P1;

            OnGameLoaded(_phase);
        }
        /// <summary>
        /// Saves the game to a file
        /// </summary>
        /// <param name="path">Path of the file</param>
        public void SaveGame(string path)
        {
            _fileManager.Save(path, _p1, _p2, _instructionIndex);
        }
        /// <summary>
        /// Processes the next pair of instructions
        /// </summary>
        /// <exception cref="InvalidOperationException"></exception>
        public void Next()
        {
            if (_phase != Phase.Fight) throw new InvalidOperationException("The fight phase has not begun");
            if (_instructionIndex == _p1.GetMaxInstructions) throw new InvalidOperationException("All instructions have been processed");

            Instruction ins1 = _p1.GetInstruction(_instructionIndex);
            Instruction ins2 = _p2.GetInstruction(_instructionIndex);

            int priority1 = ins1.GetPriority;
            int priority2 = ins2.GetPriority;

            OnInstructionProcessed(ins1, ins2);
            _instructionIndex++;

            if (priority1 == priority2) ProcessParralel(ins1, ins2);
            else if (priority1 < priority2)
            {
                Process(ins1, true);
                if (!_p2.IsDead) Process(ins2, false);
            }
            else
            {
                Process(ins2, false);
                if (!_p1.IsDead) Process(ins1, true);
            }
        }
        /// <summary>
        /// Adds the instruction to the array of instructions of the active player
        /// </summary>
        /// <param name="ins">Instruction to be added</param>
        /// <exception cref="InvalidOperationException"></exception>
        public void AddInstruction(Instruction ins)
        {
            if (GetIsP1Active)
            {
                _p1.AddInstruction(ins);
            }
            else if (GetIsP2Active)
            {
                _p2.AddInstruction(ins);
            }
            else throw new InvalidOperationException("Instructions cannot be added during the fight phase");

            OnInstructionAdded(ins);
        }
        /// <summary>
        /// Removes the last instruction from the array of instructions of the active player 
        /// </summary>
        /// <exception cref="InvalidOperationException"></exception>
        public void RemoveInstruction()
        {
            if (GetIsP1Active)
            {
                _p1.RemoveInstruction();
            }
            else if (GetIsP2Active)
            {
                _p2.RemoveInstruction();
            }
            else throw new InvalidOperationException("Instructions cannot be removed during the fight phase");

            OnInstructionRemoved();
        }
        /// <summary>
        /// Locks the array of instructions of a plyaer or resets it for both players, and changes the game phase
        /// </summary>
        /// <exception cref="InvalidOperationException"></exception>
        public void Confirm()
        {
            if (GetIsP1Active)
            {
                _p1.Locked = true;

                _phase = Phase.P2;
            }
            else if (GetIsP2Active)
            {
                _p2.Locked = true;

                _phase = Phase.Fight;
            }
            else
            {
                if (_instructionIndex != _p1.GetMaxInstructions) throw new InvalidOperationException("Not all instructions have been processed");

                _p1.ResetInstructions();
                _p2.ResetInstructions();

                _phase = Phase.P1;
                _instructionIndex = 0;
            }

            OnPhaseChanged(_phase);
        }
        #endregion

        #region Private game methods
        //tfh a lépés előnyt élvez a támadással szemben
        //    az ütés              a lövéssel
        //többi mindegy
        private void ProcessParralel(Instruction ins1, Instruction ins2)
        {
            if (ins1 is Step step1 && ins2 is Step step2)
            {
                Coordinate deltaCoord1 = step1.SimulateStep();
                Coordinate deltaCoord2 = step2.SimulateStep();

                Coordinate startCoord1 = _p1.GetCoordinate;
                Coordinate startCoord2 = _p2.GetCoordinate;

                Coordinate tempCoord1 = startCoord1 + deltaCoord1;
                Coordinate tempCoord2 = startCoord2 + deltaCoord2;

                tempCoord1.InBounds(_size);
                tempCoord2.InBounds(_size);

                if (tempCoord1 != tempCoord2)
                {
                    _p1.StepTo(deltaCoord1);
                    _p2.StepTo(deltaCoord2);

                    if (tempCoord1 == startCoord2)
                    {
                        OnPlayerStepped(startCoord2, _p2.GetCoordinate);
                        OnPlayerStepped(startCoord1, _p1.GetCoordinate);
                    }
                    else
                    {
                        OnPlayerStepped(startCoord1, _p1.GetCoordinate);
                        OnPlayerStepped(startCoord2, _p2.GetCoordinate);
                    }
                }
            }
            else if (ins1 is Turn turn1 && ins2 is Turn turn2)
            {
                _p1.Turn(turn1.GetRotation);
                _p2.Turn(turn2.GetRotation);

                OnPlayerTurned(_p1.GetCoordinate, _p1.GetDirection);
                OnPlayerTurned(_p2.GetCoordinate, _p2.GetDirection);
            }
            else if (ins1 is Attack attack1 && ins2 is Attack attack2)
            {
                bool p2WouldBeHit = attack1.WouldBeHit(_p1.GetCoordinate, _p2.GetCoordinate, _p1.GetDirection);
                bool p1WouldBeHit = attack2.WouldBeHit(_p2.GetCoordinate, _p1.GetCoordinate, _p2.GetDirection);

                if (p2WouldBeHit) _p2.TakeDamage();
                if (p1WouldBeHit) _p1.TakeDamage();

                if (p2WouldBeHit || p1WouldBeHit) OnBothPlayersDamaged(_p1.GetHp, _p2.GetHp);
            }
        }
        //ins1 mindig p1 utasítása
        private void Process(Instruction ins, bool isPlayer1)
        {
            Player localP1;
            Player localP2;

            if (isPlayer1)
            {
                localP1 = _p1;
                localP2 = _p2;
            }
            else
            {
                localP1 = _p2;
                localP2 = _p1;
            }

            if (ins is Step step)
            {
                Coordinate deltaCoord = step.SimulateStep();
                Coordinate startCoord = localP1.GetCoordinate;

                if (startCoord + deltaCoord != localP2.GetCoordinate)
                {
                    localP1.StepTo(deltaCoord);

                    OnPlayerStepped(startCoord, localP1.GetCoordinate);
                }
            }
            else if (ins is Turn turn)
            {
                localP1.Turn(turn.GetRotation);

                OnPlayerTurned(localP1.GetCoordinate, localP1.GetDirection);
            }
            else if (ins is Attack attack)
            {
                if (attack.WouldBeHit(localP1.GetCoordinate, localP2.GetCoordinate, localP1.GetDirection))
                {
                    localP2.TakeDamage();
                    OnPlayerDamaged(!isPlayer1, localP2.GetHp);
                }
            }
        }
        #endregion

        #region Private event methods
        private void OnPlayerStepped(Coordinate startCoord, Coordinate endCoord)
        {
            PlayerStepped?.Invoke(this, new StepEventArgs(startCoord, endCoord));
        }
        private void OnPlayerTurned(Coordinate coord, Direction newDir)
        {
            PlayerTurned?.Invoke(this, new TurnEventArgs(coord, newDir));
        }
        private void OnPlayerDamaged(bool isPlayer1, int newHp)
        {
            PlayerDamaged?.Invoke(this, new DamageEventArgs(isPlayer1, newHp));
            if (newHp == 0)
            {
                if (isPlayer1) OnGameOver(false);
                else OnGameOver(true);
            }
        }
        private void OnBothPlayersDamaged(int newHp1, int newHp2)
        {
            PlayerDamaged?.Invoke(this, new DamageEventArgs(true, newHp1));
            PlayerDamaged?.Invoke(this, new DamageEventArgs(false, newHp2));

            if (newHp1 == 0 && newHp2 == 0)
            {
                OnGameOver(null);
            }
            else if (newHp1 == 0)
            {
                OnGameOver(false);
            }
            else if (newHp2 == 0)
            {
                OnGameOver(true);
            }
        }
        private void OnGameOver(bool? winner)
        {
            GameOver?.Invoke(this, new GameOverEventArgs(winner, _p1.GetCoordinate, _p2.GetCoordinate));
        }
        private void OnInstructionProcessed(Instruction ins1, Instruction ins2)
        {
            InstructionProcessed?.Invoke(this, new InstructionEventArgs(ins1, ins2));
        }
        private void OnInstructionAdded(Instruction ins)
        {
            InstructionAdded?.Invoke(this, new InstructionEventArgs(ins));
        }
        private void OnInstructionRemoved()
        {
            InstructionRemoved?.Invoke(this, EventArgs.Empty);
        }
        private void OnPhaseChanged(Phase newPhase)
        {
            PhaseChanged?.Invoke(this, new PhaseEventArgs(newPhase, _p1.GetCoordinate, _p2.GetCoordinate));
        }
        private void OnGameLoaded(Phase newPhase)
        {
            GameLoaded?.Invoke(this, new PhaseEventArgs(newPhase, _p1.GetCoordinate, _p2.GetCoordinate));
        }
        #endregion
    }
}