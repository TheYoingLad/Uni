using LaserPigs.Model;
using LaserPigs.Persistence;
using LaserPigs.Persistence.Instructions;
using System.Text;

namespace LaserPigs.View
{
    public partial class LaserPigsForm : Form
    {
        #region Constants
        private const int GridSizeConst = 600;
        private const string UpConst = "˄\nO";
        private const string RightConst = "O˃";
        private const string DownConst = "O\n˅";
        private const string LeftConst = "˂O";
        private const string HpConst = "█ ";
        private const int P1ColourConst = -16776961;
        private const int P2ColourConst = -65536;
        private const int HpHighConst = -13447886;
        private const int HpMediumConst = -23296;
        private const int HpLowConst = -65536;
        #endregion

        #region Fields
        private LaserPigsModel _model = null!;
        private Label[,] _map = null!;
        #endregion

        #region Constructors
        /// <summary>
        /// Initialises the game
        /// </summary>
        public LaserPigsForm()
        {
            InitializeComponent();

            IFileManager fileManager = new FileManager();

            _model = new LaserPigsModel(fileManager);
            _model.PlayerStepped += Model_PlayerStep;
            _model.PlayerTurned += Model_PlayerTurn;
            _model.PlayerDamaged += Model_PlayerDamage;
            _model.GameOver += Model_GameOver;
            _model.InstructionProcessed += Model_InstructionProcessed;
            _model.InstructionAdded += Model_InstructionAdd;
            _model.InstructionRemoved += Model_InstructionRemove;
            _model.PhaseChanged += Model_PhaseChange;
            _model.GameLoaded += Model_GameLoad;

            _labelP1Colour.ForeColor = Color.FromArgb(P1ColourConst);
            _labelP2Colour.ForeColor = Color.FromArgb(P2ColourConst);

            GenerateMap();

            SetupMenus();

            ResetGame();
        }
        #endregion

        #region Button event handlers
        private void Button_Delete_Click(object sender, EventArgs e)
        {
            if (sender is Button b)
            {
                try
                {
                    _model.RemoveInstruction();
                }
                catch (Exception ex)
                {
                    MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                }
            }
        }
        private void Button_Up_Click(object sender, EventArgs e)
        {
            if (sender is Button b)
            {
                Instruction ins = new Step(Direction.Up);
                TryAddInstruction(ins);
            }
        }
        private void Button_Right_Click(object sender, EventArgs e)
        {
            if (sender is Button b)
            {
                Instruction ins = new Step(Direction.Right);
                TryAddInstruction(ins);
            }
        }
        private void Button_Down_Click(object sender, EventArgs e)
        {
            if (sender is Button b)
            {
                Instruction ins = new Step(Direction.Down);
                TryAddInstruction(ins);
            }
        }
        private void Button_Left_Click(object sender, EventArgs e)
        {
            if (sender is Button b)
            {
                Instruction ins = new Step(Direction.Left);
                TryAddInstruction(ins);
            }
        }
        private void Button_CW_Click(object sender, EventArgs e)
        {
            if (sender is Button b)
            {
                Instruction ins = new Turn(Rotation.CW);
                TryAddInstruction(ins);
            }
        }
        private void Button_CCW_Click(object sender, EventArgs e)
        {
            if (sender is Button b)
            {
                Instruction ins = new Turn(Rotation.CCW);
                TryAddInstruction(ins);
            }
        }
        private void Button_Punch_Click(object sender, EventArgs e)
        {
            if (sender is Button b)
            {
                Instruction ins = new Punch();
                TryAddInstruction(ins);
            }
        }
        private void Button_Shoot_Click(object sender, EventArgs e)
        {
            if (sender is Button b)
            {
                Instruction ins = new Shoot();
                TryAddInstruction(ins);
            }
        }
        private void Button_Confirm_Click(object sender, EventArgs e)
        {
            if (sender is Button b)
            {
                try
                {
                    _model.Confirm();
                }
                catch (Exception ex)
                {
                    MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                }
            }
        }
        private void Button_Next_Click(object sender, EventArgs e)
        {
            if (sender is Button b)
            {
                try
                {
                    _model.Next();
                }
                catch (Exception ex)
                {
                    MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                }
            }
        }
        #endregion

        #region Model event handlers
        private void Model_PlayerStep(object? sender, StepEventArgs e)
        {
            Coordinate startCoord = e.GetStartCoordinate;
            int startX = startCoord.GetX;
            int startY = startCoord.GetY;

            Coordinate endCoord = e.GetEndCoordinate;
            int endX = endCoord.GetX;
            int endY = endCoord.GetY;

            string playerText = _map[startX, startY].Text;
            Color playerColour = _map[startX, startY].ForeColor;

            _map[startX, startY].Text = "";

            _map[endX, endY].Text = playerText;
            _map[endX, endY].ForeColor = playerColour;
        }
        private void Model_PlayerTurn(object? sender, TurnEventArgs e)
        {
            Coordinate coord = e.GetCoordinate;
            int x = coord.GetX;
            int y = coord.GetY;

            switch (e.GetNewDirection)
            {
                case Direction.Up:
                    _map[x, y].Text = UpConst;
                    break;
                case Direction.Right:
                    _map[x, y].Text = RightConst;
                    break;
                case Direction.Down:
                    _map[x, y].Text = DownConst;
                    break;
                case Direction.Left:
                    _map[x, y].Text = LeftConst;
                    break;
            }
        }
        private void Model_PlayerDamage(object? sender, DamageEventArgs e)
        {
            RefreshHp(e.GetIsPlayer1, e.GetNewHp);
        }
        private void Model_GameOver(object? sender, GameOverEventArgs e)
        {
            switch (e.GetWinner)
            {
                case true:
                    MessageBox.Show("Player 1 won!", "Game Over", MessageBoxButtons.OK, MessageBoxIcon.Information);
                    break;
                case false:
                    MessageBox.Show("Player 2 won!", "Game Over", MessageBoxButtons.OK, MessageBoxIcon.Information);
                    break;
                default:
                    MessageBox.Show("Draw!", "Game Over", MessageBoxButtons.OK, MessageBoxIcon.Information);
                    break;
            }

            if (MessageBox.Show("Play Again?", "New Game", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                WipeCell(e.GetCoord1);
                WipeCell(e.GetCoord2);

                ResetGame();
            }
            else this.Close();
        }
        private void Model_InstructionProcessed(object? sender, InstructionEventArgs e)
        {
            Instruction ins1 = e.GetInstruction;
            Instruction? ins2 = e.GetInstruction2;

            if (ins2 == null) throw new ArgumentException("The second instruction is missing", nameof(ins2));
            int index = _model.GetInstructionIndex;

            _listBoxP1Moves.Items.RemoveAt(index);
            _listBoxP1Moves.Items.Insert(index, ins1.ToString()!);

            _listBoxP2Moves.Items.RemoveAt(index);
            _listBoxP2Moves.Items.Insert(index, ins2.ToString()!);

            _labelInstructionsRemainingValue.Text = (int.Parse(_labelInstructionsRemainingValue.Text) - 1).ToString();
        }
        private void Model_InstructionAdd(object? sender, InstructionEventArgs e)
        {
            if (_model.GetIsP1Active) _listBoxP1Moves.Items.Add(e.GetInstruction.ToString()!);
            else _listBoxP2Moves.Items.Add(e.GetInstruction.ToString()!);

            _labelInstructionsRemainingValue.Text = (int.Parse(_labelInstructionsRemainingValue.Text) - 1).ToString();
        }
        private void Model_InstructionRemove(object? sender, EventArgs e)
        {
            if (_model.GetIsP1Active) _listBoxP1Moves.Items.RemoveAt(_listBoxP1Moves.Items.Count - 1);
            else _listBoxP2Moves.Items.RemoveAt(_listBoxP2Moves.Items.Count - 1);

            _labelInstructionsRemainingValue.Text = (int.Parse(_labelInstructionsRemainingValue.Text) + 1).ToString();
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
            Instruction[] instructions;
            int instructionCount;

            SetupPhase(e.GetNewPhase);

            switch (e.GetNewPhase)
            {
                case Phase.P1:
                    instructions = _model.GetP1Instructions;
                    instructionCount = _model.GetP1InstructionCount;

                    for (int i = 0; i < instructionCount; i++) _listBoxP1Moves.Items.Add(instructions[i].ToString()!);

                    _labelInstructionsRemainingValue.Text = (_model.GetMaxInstructions - instructionCount).ToString();
                    break;
                case Phase.P2:
                    instructions = _model.GetP2Instructions;
                    instructionCount = _model.GetP2InstructionCount;

                    for (int i = 0; i < instructionCount; i++) _listBoxP2Moves.Items.Add(instructions[i].ToString()!);

                    _labelInstructionsRemainingValue.Text = (_model.GetMaxInstructions - instructionCount).ToString();
                    break;
                case Phase.Fight:
                    instructions = _model.GetP1Instructions;
                    instructionCount = _model.GetInstructionIndex;

                    for (int i = 0; i < instructionCount; i++)
                    {
                        _listBoxP1Moves.Items.RemoveAt(i);
                        _listBoxP1Moves.Items.Insert(i, instructions[i].ToString()!);

                    }

                    instructions = _model.GetP2Instructions;

                    for (int i = 0; i < instructionCount; i++)
                    {
                        _listBoxP2Moves.Items.RemoveAt(i);
                        _listBoxP2Moves.Items.Insert(i, instructions[i].ToString()!);

                    }

                    _labelInstructionsRemainingValue.Text = (_model.GetMaxInstructions - instructionCount).ToString();
                    break;
            }
        }
        #endregion

        #region Menu event handlers
        private void Menu_NewGame4X_Click(object sender, EventArgs e)
        {
            NewGamePrompt(4);
        }
        private void Menu_NewGame6X_Click(object sender, EventArgs e)
        {
            NewGamePrompt(6);
        }
        private void Menu_NewGame8X_Click(object sender, EventArgs e)
        {
            NewGamePrompt(8);
        }
        private void Menu_SaveGame_Click(object sender, EventArgs e)
        {
            using (SaveFileDialog saveFileDialog = new SaveFileDialog())
            {
                saveFileDialog.InitialDirectory = Application.StartupPath;
                saveFileDialog.RestoreDirectory = true;
                saveFileDialog.Filter = "LaserPig files (*.lspf)|*.lspf";

                if (saveFileDialog.ShowDialog() == DialogResult.OK)
                {
                    try
                    {
                        _model.SaveGame(saveFileDialog.FileName);
                    }
                    catch (Exception ex)
                    {
                        MessageBox.Show("An error occured:\n" + ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                    }

                }
            }
        }
        private void Menu_LoadGame_Click(object sender, EventArgs e)
        {
            using (OpenFileDialog openFileDialog = new OpenFileDialog())
            {
                openFileDialog.InitialDirectory = Application.StartupPath;
                openFileDialog.RestoreDirectory = true;
                openFileDialog.Filter = "LaserPig files (*.lspf)|*.lspf";

                if (openFileDialog.ShowDialog() == DialogResult.OK)
                {
                    try
                    {
                        _model.LoadGame(openFileDialog.FileName);
                    }
                    catch (Exception ex)
                    {
                        MessageBox.Show("An error occured:\n" + ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                        MessageBox.Show("File could not be loaded, restarting game", "Restart", MessageBoxButtons.OK, MessageBoxIcon.Information); //redo

                        _model.NewGame();
                        SetupPhase(Phase.P1);
                    }

                    foreach (Label label in _map) Controls.Remove(label);

                    GenerateMap();

                    SetupMenus();
                    SetupMap();

                    SetupHealthBars();
                    RefreshHp(true, _model.GetP1Hp);
                    RefreshHp(false, _model.GetP2Hp);
                }
            }
        }
        private void Menu_Exit_Click(object sender, EventArgs e)
        {
            if (MessageBox.Show("Are you sure you want to quit?", "Exit", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                this.Close();
            }
        }
        #endregion

        #region Private setup methods
        private void GenerateMap()
        {
            int size = _model.Size;
            int baseX = _menuStrip.ClientSize.Height;
            int baseY = 0;

            _map = new Label[size, size];

            for (int i = 0; i < size; i++)
            {
                for (int j = 0; j < size; j++)
                {

                    _map[i, j] = new Label();
                    _map[i, j].Location = new Point(baseY + 20 + (GridSizeConst / size) * j, baseX + 40 + (GridSizeConst / size) * i);
                    _map[i, j].Size = new Size(GridSizeConst / size, GridSizeConst / size);
                    _map[i, j].BackColor = Color.LightGreen;
                    _map[i, j].TextAlign = ContentAlignment.MiddleCenter;
                    _map[i, j].BorderStyle = BorderStyle.FixedSingle;
                    _map[i, j].Font = new Font(FontFamily.GenericSansSerif, (GridSizeConst / size) / 4);

                    Controls.Add(_map[i, j]);
                }
            }
        }
        private void SetupMenus()
        {
            _menuItemX4.Checked = (_model.Size == 4);
            _menuItemX6.Checked = (_model.Size == 6);
            _menuItemX8.Checked = (_model.Size == 8);
        }
        private void SetupMap()
        {
            Coordinate p1Coord = _model.GetP1Coord;
            int p1X = p1Coord.GetX;
            int p1Y = p1Coord.GetY;
            Direction d1 = _model.GetP1Direction;

            Coordinate p2Coord = _model.GetP2Coord;
            int p2X = p2Coord.GetX;
            int p2Y = p2Coord.GetY;
            Direction d2 = _model.GetP2Direction;

            _map[p1X, p1Y].Text = GetStringDirection(d1);
            _map[p1X, p1Y].ForeColor = Color.FromArgb(P1ColourConst);
            if (_model.GetIsP1Active) HighlightCell(p1Coord);

            _map[p2X, p2Y].Text = GetStringDirection(d2);
            _map[p2X, p2Y].ForeColor = Color.FromArgb(P2ColourConst);
            if (_model.GetIsP2Active) HighlightCell(p2Coord);
        }
        private void SetupPhase(Phase phase)
        {
            _listBoxP1Moves.Items.Clear();
            _listBoxP2Moves.Items.Clear();

            int maxInstructions = _model.GetMaxInstructions;

            switch (phase)
            {
                case Phase.P1:
                    for (int i = 0; i < maxInstructions; i++) _listBoxP2Moves.Items.Add("??????????");

                    _labelInstructionsRemainingValue.Text = maxInstructions.ToString();
                    _labelGamePhase.Text = "Phase: Player 1";
                    break;
                case Phase.P2:
                    for (int i = 0; i < maxInstructions; i++) _listBoxP1Moves.Items.Add("??????????");

                    _labelInstructionsRemainingValue.Text = maxInstructions.ToString();
                    _labelGamePhase.Text = "Phase: Player 2";
                    break;
                case Phase.Fight:
                    for (int i = 0; i < maxInstructions; i++) _listBoxP1Moves.Items.Add("??????????");
                    for (int i = 0; i < maxInstructions; i++) _listBoxP2Moves.Items.Add("??????????");

                    _labelInstructionsRemainingValue.Text = maxInstructions.ToString();
                    _labelGamePhase.Text = "Phase: Fight";
                    break;
            }
        }
        private void SetupHealthBars()
        {
            int hp1 = _model.GetP1MaxHp;
            int hp2 = _model.GetP2MaxHp;
            StringBuilder sb = new StringBuilder();

            _statusStripHealthBar1.ForeColor = Color.FromArgb(HpHighConst);
            //_statusStripHealthBar1.Width = 20 * hp1;
            for (int i = 0; i < hp1; i++) sb.Append(HpConst);
            _statusStripHealthBar1.Text = sb.ToString();

            sb.Clear();
            _statusStripHealthBar2.ForeColor = Color.FromArgb(HpHighConst);
            //_statusStripHealthBar2.Width = 20 * hp2;
            for (int i = 0; i < hp2; i++) sb.Append(HpConst);
            _statusStripHealthBar2.Text = sb.ToString();
        }
        private void ResetGame()
        {
            _model.NewGame();
            SetupMap();

            SetupPhase(Phase.P1);

            SetupHealthBars();
        }
        #endregion

        #region Private cell methods
        private void HighlightCell(Coordinate coord)
        {
            int x = coord.GetX;
            int y = coord.GetY;

            _map[x, y].BackColor = Color.LightGray;
        }
        private void UnHighlightCell(Coordinate coord)
        {
            int x = coord.GetX;
            int y = coord.GetY;

            _map[x, y].BackColor = Color.LightGreen;
        }
        private void WipeCell(Coordinate coord)
        {
            int x = coord.GetX;
            int y = coord.GetY;

            _map[x, y].Text = "";
        }
        #endregion

        #region Private helper methods
        private void TryAddInstruction(Instruction ins)
        {
            try
            {
                _model.AddInstruction(ins);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }
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
        private void RefreshHp(bool isP1, int newHp)
        {
            int maxHp;
            StringBuilder sb = new StringBuilder();

            for (int i = 0; i < newHp; i++) sb.Append(HpConst);

            if (isP1)
            {
                _statusStripHealthBar1.Text = sb.ToString();

                maxHp = _model.GetP1MaxHp;
                if ((newHp * 100) / maxHp < 34) _statusStripHealthBar1.ForeColor = Color.FromArgb(HpLowConst);
                else if ((newHp * 100) / maxHp < 67) _statusStripHealthBar1.ForeColor = Color.FromArgb(HpMediumConst);
                else _statusStripHealthBar1.ForeColor = Color.FromArgb(HpHighConst);
            }
            else
            {
                _statusStripHealthBar2.Text = sb.ToString();

                maxHp = _model.GetP2MaxHp;
                if ((newHp * 100) / maxHp < 34) _statusStripHealthBar2.ForeColor = Color.FromArgb(HpLowConst);
                else if ((newHp * 100) / maxHp < 67) _statusStripHealthBar2.ForeColor = Color.FromArgb(HpMediumConst);
                else _statusStripHealthBar2.ForeColor = Color.FromArgb(HpHighConst);
            }
        }
        private void NewGamePrompt(int n)
        {
            if (MessageBox.Show($"Start new game?\n\nNew grid size: {n}", "New Game", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                _model.Size = n;

                foreach (Label label in _map) Controls.Remove(label);

                GenerateMap();

                ResetGame();
            }
            SetupMenus();
        }
        #endregion
    }
}
