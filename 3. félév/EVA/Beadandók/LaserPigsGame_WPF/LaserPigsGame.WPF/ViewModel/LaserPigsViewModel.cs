using LaserPigs.Model;
using LaserPigs.Persistence;
using LaserPigs.Persistence.Instructions;
using System.Collections.ObjectModel;
using System.Text;
using System.Windows.Media;

namespace LaserPigsGame.ViewModel
{
    /// <summary>
    /// The viewmodel for the Laser Pigs game
    /// </summary>
    public class LaserPigsViewModel : ViewModelBase
    {
        #region Constants
        private const string UpConst = "˄\nO";
        private const string RightConst = "O˃";
        private const string DownConst = "O\n˅";
        private const string LeftConst = "˂O";
        private const string HpConst = "█ ";
        #endregion

        #region Fields
        private LaserPigsModel _model;
        #endregion

        #region Constructor
        /// <summary>
        /// Constructs the viewmodel
        /// </summary>
        /// <param name="model">The actual game model</param>
        public LaserPigsViewModel(LaserPigsModel model)
        {
            _model = model;
            _model.PlayerStepped += Model_PlayerStep;
            _model.PlayerTurned += Model_PlayerTurn;
            _model.PlayerDamaged += Model_PlayerDamage;
            _model.InstructionProcessed += Model_InstructionProcessed;
            _model.InstructionAdded += Model_InstructionAdd;
            _model.InstructionRemoved += Model_InstructionRemove;
            _model.PhaseChanged += Model_PhaseChange;
            _model.GameLoaded += Model_GameLoad;
            _model.PhaseChanged += Model_PhaseChange;

            DeleteCommand = new DelegateCommand(_ => _model.RemoveInstruction());
            AddCommand = new DelegateCommand(param => { if (param is string s) _model.AddInstruction(GetInstructionFromInt(int.Parse(s))); });
            ConfirmCommand = new DelegateCommand(_ => _model.Confirm());
            NextCommand = new DelegateCommand(_ => _model.Next());
            NewGameCommand = new DelegateCommand(param => { if (param is string s) OnNewGame(int.Parse(s)); });
            LoadGameCommand = new DelegateCommand(_ => OnLoadGame());
            SaveGameCommand = new DelegateCommand(_ => OnSaveGame());
            ExitGameCommand = new DelegateCommand(_ => OnExitGame());

            Map = new ObservableCollection<LaserPigsCell>();
            GenerateMap();

            SetupMap();

            P1Instructions = new ObservableCollection<string>();
            P2Instructions = new ObservableCollection<string>();
            SetupPhase(Phase.P1);

            RefreshHp(true);
            RefreshHp(false);
        }
        #endregion

        #region Properties
        /// <summary>
        /// Gets the font size for the cells. Proportional to the amount of cells
        /// </summary>
        public int FontSize => MapSize / Size / 4;
        /// <summary>
        /// Gets the size of the map in pixels
        /// </summary>
        public int MapSize => 600;
        /// <summary>
        /// Gets the size of the map in the amount of rows and columns
        /// </summary>
        public int Size
        {
            get => _model.Size;
        }
        /// <summary>
        /// Gets the amount of instructions that need to be given
        /// </summary>
        public int InstructionsRemainig
        {
            get
            {
                if (_model.GetIsP1Active)
                {
                    return _model.GetMaxInstructions - _model.GetP1InstructionCount;
                }
                else if (_model.GetIsP2Active)
                {
                    return _model.GetMaxInstructions - _model.GetP2InstructionCount;
                }
                else return 0;
            }
        }
        /// <summary>
        /// Gets whether the the Confirm method of the model can be called
        /// </summary>
        public bool CanConfirm
        {
            get
            {
                if (_model.GetIsP1Active)
                {
                    return _model.GetP1InstructionCount == _model.GetMaxInstructions;
                }
                else if (_model.GetIsP2Active)
                {
                    return _model.GetP2InstructionCount == _model.GetMaxInstructions;
                }
                else
                {
                    return _model.GetInstructionIndex == _model.GetMaxInstructions - 1;
                }
            }
        }
        /// <summary>
        /// Gets whether the the Next method of the model can be called
        /// </summary>
        public bool CanNext
        {
            get
            {
                if (!_model.GetIsP1Active && !_model.GetIsP2Active)
                {
                    return _model.GetInstructionIndex != _model.GetMaxInstructions - 1;
                }
                else
                {
                    return false;
                }
            }
        }
        /// <summary>
        /// Gets whether the the RemoveInstruction method of the model can be called
        /// </summary>
        public bool CanDelete
        {
            get
            {
                if (IsInstructionPhase)
                {
                    return _model.GetIsP1Active ? _model.GetP1InstructionCount != 0 : _model.GetP2InstructionCount != 0;
                }
                else return false;
            }
        }
        /// <summary>
        /// Gets whether the the AddInstruction method of the model can be called
        /// </summary>
        public bool CanAdd
        {
            get
            {
                if (IsInstructionPhase)
                {
                    return _model.GetIsP1Active ? _model.GetP1InstructionCount != _model.GetMaxInstructions : _model.GetP2InstructionCount != _model.GetMaxInstructions;
                }
                else return false;
            }
        }
        /// <summary>
        /// Gets whether the phase is P1 or P2 in the model
        /// </summary>
        public bool IsInstructionPhase
        {
            get
            {
                return _model.GetIsP1Active || _model.GetIsP2Active;
            }
        }
        /// <summary>
        /// Gets whether the map is 4x4
        /// </summary>
        public bool Is4x4
        {
            get => Size == 4;
        }
        /// <summary>
        /// Gets whether the map is 6x6
        /// </summary>
        public bool Is6x6
        {
            get => Size == 6;
        }
        /// <summary>
        /// Gets whether the map is 8x8
        /// </summary>
        public bool Is8x8
        {
            get => Size == 8;
        }
        /// <summary>
        /// Gets the text equivalent of the current phase of the model
        /// </summary>
        public string PhaseLabel
        {
            get
            {
                if (_model.GetIsP1Active) return "Player 1";
                else if (_model.GetIsP2Active) return "Player 2";
                else return "Fight";
            }
        }
        /// <summary>
        /// Gets the remaining hp of player 1
        /// </summary>
        public string P1Hp
        {
            get
            {
                StringBuilder sb = new StringBuilder();

                for (int i = 0; i < _model.GetP1Hp; i++) sb.Append(HpConst);

                return sb.ToString();
            }
        }
        /// <summary>
        /// Gets the remaining hp of player 2
        /// </summary>
        public string P2Hp
        {
            get
            {
                StringBuilder sb = new StringBuilder();

                for (int i = 0; i < _model.GetP2Hp; i++) sb.Append(HpConst);

                return sb.ToString();
            }
        }
        /// <summary>
        /// Gets the colour of player 1
        /// </summary>
        public SolidColorBrush P1Colour
        {
            get
            {
                return Brushes.Blue;
            }
        }
        /// <summary>
        /// Gets the colour of player 2
        /// </summary>
        public SolidColorBrush P2Colour
        {
            get
            {
                return Brushes.Red;
            }
        }
        /// <summary>
        /// Gets the colour of the hp bar of player 1
        /// </summary>
        public SolidColorBrush P1HpColour
        {
            get
            {
                if ((_model.GetP1Hp * 100) / _model.GetP1MaxHp < 34) return Brushes.Red;
                else if ((_model.GetP1Hp * 100) / _model.GetP1MaxHp < 67) return Brushes.Orange;
                else return Brushes.LimeGreen;
            }
        }
        /// <summary>
        /// Gets the colour of the hp bar of player 2
        /// </summary>
        public SolidColorBrush P2HpColour
        {
            get
            {
                if ((_model.GetP2Hp * 100) / _model.GetP2MaxHp < 34) return Brushes.Red;
                else if ((_model.GetP2Hp * 100) / _model.GetP2MaxHp < 67) return Brushes.Orange;
                else return Brushes.LimeGreen;
            }
        }
        /// <summary>
        /// Gets ot sets the map
        /// </summary>
        public ObservableCollection<LaserPigsCell> Map { get; set; }
        /// <summary>
        /// Gets or sets the list of the text representation of the instructions of player 1
        /// </summary>
        public ObservableCollection<string> P1Instructions { get; set; }
        /// <summary>
        /// Gets or sets the list of the text representation of the instructions of player 2
        /// </summary>
        public ObservableCollection<string> P2Instructions { get; set; }
        #endregion

        #region Commands
        /// <summary>
        /// Command for deleting an instruction
        /// </summary>
        public DelegateCommand DeleteCommand { get; private set; }
        /// <summary>
        /// Command for adding an instruction
        /// </summary>
        public DelegateCommand AddCommand { get; private set; }
        /// <summary>
        /// Command for confirming the instructions
        /// </summary>
        public DelegateCommand ConfirmCommand { get; private set; }
        /// <summary>
        /// Command for processing the next instruction
        /// </summary>
        public DelegateCommand NextCommand { get; private set; }
        /// <summary>
        /// Command for starting a new game
        /// </summary>
        public DelegateCommand NewGameCommand { get; private set; }
        /// <summary>
        /// Command for loading a game
        /// </summary>
        public DelegateCommand LoadGameCommand { get; private set; }
        /// <summary>
        /// Command for saving a game
        /// </summary>
        public DelegateCommand SaveGameCommand { get; private set; }
        /// <summary>
        /// Command for exiting a game
        /// </summary>
        public DelegateCommand ExitGameCommand { get; private set; }
        #endregion

        #region Events
        /// <summary>
        /// Event of starting a new game
        /// </summary>
        public event EventHandler<int>? NewGame;
        /// <summary>
        /// Event of loading a game
        /// </summary>
        public event EventHandler? LoadGame;
        /// <summary>
        /// Event of saving a game
        /// </summary>
        public event EventHandler? SaveGame;
        /// <summary>
        /// Event of exiting a game
        /// </summary>
        public event EventHandler? ExitGame;
        #endregion

        #region Event methods
        private void OnNewGame(int n)
        {
            NewGame?.Invoke(this, n);
        }
        private void OnLoadGame()
        {
            LoadGame?.Invoke(this, EventArgs.Empty);
        }
        private void OnSaveGame()
        {
            SaveGame?.Invoke(this, EventArgs.Empty);
        }
        private void OnExitGame()
        {
            ExitGame?.Invoke(this, EventArgs.Empty);
        }
        #endregion

        #region Model event handlers
        private void Model_PlayerStep(object? sender, StepEventArgs e)
        {
            Coordinate startCoord = e.GetStartCoordinate;
            LaserPigsCell startCell = Map.Single(c => c.Coordinate == startCoord);

            Coordinate endCoord = e.GetEndCoordinate;
            LaserPigsCell endCell = Map.Single(c => c.Coordinate == endCoord);

            string playerText = startCell.Text;
            SolidColorBrush playerColour = startCell.TextColour;

            startCell.Text = "";

            endCell.Text = playerText;
            endCell.TextColour = playerColour;
        }
        private void Model_PlayerTurn(object? sender, TurnEventArgs e)
        {
            Coordinate coord = e.GetCoordinate;
            LaserPigsCell cell = Map.Single(c => c.Coordinate == coord);

            switch (e.GetNewDirection)
            {
                case Direction.Up:
                    cell.Text = UpConst;
                    break;
                case Direction.Right:
                    cell.Text = RightConst;
                    break;
                case Direction.Down:
                    cell.Text = DownConst;
                    break;
                case Direction.Left:
                    cell.Text = LeftConst;
                    break;
            }
        }
        private void Model_PlayerDamage(object? sender, DamageEventArgs e)
        {
            RefreshHp(e.GetIsPlayer1);
        }
        private void Model_InstructionProcessed(object? sender, InstructionEventArgs e)
        {
            Instruction ins1 = e.GetInstruction;
            Instruction? ins2 = e.GetInstruction2;

            if (ins2 == null) throw new ArgumentException("The second instruction is missing", nameof(ins2));
            int index = _model.GetInstructionIndex;

            P1Instructions.RemoveAt(index);
            P1Instructions.Insert(index, ins1.ToString()!);

            P2Instructions.RemoveAt(index);
            P2Instructions.Insert(index, ins2.ToString()!);

            OnPropertyChanged(nameof(CanConfirm));
            OnPropertyChanged(nameof(CanNext));
        }
        private void Model_InstructionAdd(object? sender, InstructionEventArgs e)
        {
            if (_model.GetIsP1Active) P1Instructions.Add(e.GetInstruction.ToString()!);
            else P2Instructions.Add(e.GetInstruction.ToString()!);

            OnPropertyChanged(nameof(InstructionsRemainig));
            OnPropertyChanged(nameof(CanAdd));
            OnPropertyChanged(nameof(CanDelete));
            OnPropertyChanged(nameof(CanConfirm));
        }
        private void Model_InstructionRemove(object? sender, EventArgs e)
        {
            if (_model.GetIsP1Active) P1Instructions.RemoveAt(P1Instructions.Count - 1);
            else P2Instructions.RemoveAt(P2Instructions.Count - 1);

            OnPropertyChanged(nameof(InstructionsRemainig));
            OnPropertyChanged(nameof(CanAdd));
            OnPropertyChanged(nameof(CanDelete));
            OnPropertyChanged(nameof(CanConfirm));
        }
        private void Model_PhaseChange(object? sender, PhaseEventArgs e)
        {
            SetupPhase(e.GetNewPhase);

            switch (e.GetNewPhase)
            {
                case Phase.P1:
                    HighlightCell(e.GetCoordinate1);
                    break;
                case Phase.P2:
                    UnHighlightCell(e.GetCoordinate1);
                    HighlightCell(e.GetCoordinate2);
                    break;
                case Phase.Fight:
                    UnHighlightCell(e.GetCoordinate2);
                    break;
            }
        }
        private void Model_GameLoad(object? sender, PhaseEventArgs e)
        {
            GenerateMap();
            SetupMap();

            SetupPhase(e.GetNewPhase);

            RefreshHp(true);
            RefreshHp(false);

            switch (e.GetNewPhase)
            {
                case Phase.P1:
                    for (int i = 0; i < _model.GetP1InstructionCount; i++) P1Instructions.Add(_model.GetP1Instructions[i].ToString()!);

                    break;
                case Phase.P2:
                    for (int i = 0; i < _model.GetP2InstructionCount; i++) P2Instructions.Add(_model.GetP2Instructions[i].ToString()!);

                    break;
                case Phase.Fight:
                    for (int i = 0; i < _model.GetInstructionIndex; i++)
                    {
                        P1Instructions.RemoveAt(i);
                        P1Instructions.Insert(i, _model.GetP1Instructions[i].ToString()!);
                    }

                    for (int i = 0; i < _model.GetInstructionIndex; i++)
                    {
                        P2Instructions.RemoveAt(i);
                        P2Instructions.Insert(i, _model.GetP2Instructions[i].ToString()!);
                    }

                    break;
            }

            OnPropertyChanged(nameof(Size));
            OnPropertyChanged(nameof(Is4x4));
            OnPropertyChanged(nameof(Is6x6));
            OnPropertyChanged(nameof(Is8x8));
        }
        #endregion

        #region Private setup methods
        private void GenerateMap()
        {
            Map.Clear();
            for (int i = 0; i < Size; i++)
            {
                for (int j = 0; j < Size; j++)
                {
                    Map.Add(new LaserPigsCell
                    {
                        Coordinate = new Coordinate(i, j),
                    });
                }
            }
        }
        private void SetupMap()
        {
            Coordinate p1Coord = _model.GetP1Coord;
            LaserPigsCell p1Cell = Map.Single(c => c.Coordinate == p1Coord);
            Direction d1 = _model.GetP1Direction;

            Coordinate p2Coord = _model.GetP2Coord;
            LaserPigsCell p2Cell = Map.Single(c => c.Coordinate == p2Coord);
            Direction d2 = _model.GetP2Direction;

            p1Cell.Text = GetStringDirection(d1);
            p1Cell.TextColour = P1Colour;
            if (_model.GetIsP1Active) HighlightCell(p1Coord);

            p2Cell.Text = GetStringDirection(d2);
            p2Cell.TextColour = P2Colour;
            if (_model.GetIsP2Active) HighlightCell(p2Coord);
        }
        private void SetupPhase(Phase phase)
        {
            P1Instructions.Clear();
            P2Instructions.Clear();

            switch (phase)
            {
                case Phase.P1:
                    for (int i = 0; i < _model.GetMaxInstructions; i++) P2Instructions.Add("??????????");
                    break;
                case Phase.P2:
                    for (int i = 0; i < _model.GetMaxInstructions; i++) P1Instructions.Add("??????????");
                    break;
                case Phase.Fight:
                    for (int i = 0; i < _model.GetMaxInstructions; i++) P1Instructions.Add("??????????");
                    for (int i = 0; i < _model.GetMaxInstructions; i++) P2Instructions.Add("??????????");
                    break;
            }

            OnPropertyChanged(nameof(IsInstructionPhase));
            OnPropertyChanged(nameof(InstructionsRemainig));
            OnPropertyChanged(nameof(PhaseLabel));
            OnPropertyChanged(nameof(CanAdd));
            OnPropertyChanged(nameof(CanDelete));
            OnPropertyChanged(nameof(CanConfirm));
            OnPropertyChanged(nameof(CanNext));
        }
        #endregion

        #region Private cell methods
        private void HighlightCell(Coordinate coord)
        {
            Map.Single(c => c.Coordinate == coord).BackColour = Brushes.LightGray;
        }
        private void UnHighlightCell(Coordinate coord)
        {
            Map.Single(c => c.Coordinate == coord).BackColour = Brushes.LightGreen;
        }
        #endregion

        #region Private helper methods
        private string GetStringDirection(Direction direction)
        {
            switch (direction)
            {
                case Direction.Up:
                    return UpConst;
                case Direction.Right:
                    return RightConst;
                case Direction.Down:
                    return DownConst;
                case Direction.Left:
                    return LeftConst;
                default:
                    throw new ArgumentOutOfRangeException(nameof(direction), "Invalid direction");
            }
        }
        private void RefreshHp(bool isP1)
        {
            if (isP1)
            {
                OnPropertyChanged(nameof(P1Hp));
                OnPropertyChanged(nameof(P1HpColour));
            }
            else
            {
                OnPropertyChanged(nameof(P2Hp));
                OnPropertyChanged(nameof(P2HpColour));
            }
        }
        private Instruction GetInstructionFromInt(int n)
        {
            switch (n)
            {
                case 0:
                    return new Step(Direction.Up);
                case 1:
                    return new Step(Direction.Right);
                case 2:
                    return new Step(Direction.Down);
                case 3:
                    return new Step(Direction.Left);
                case 4:
                    return new Turn(Rotation.CW);
                case 5:
                    return new Turn(Rotation.CCW);
                case 6:
                    return new Punch();
                case 7:
                    return new Shoot();
                default:
                    throw new ArgumentException("Invalid Instruction code", nameof(n));
            }
        }
        #endregion
    }
}
