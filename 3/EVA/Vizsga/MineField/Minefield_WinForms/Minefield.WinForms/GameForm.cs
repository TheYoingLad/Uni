using Minefield.Model;
using Minefield.Persistence;


namespace Minefield.WinForms
{
    /// <summary>
    /// Aknamez� j�t�k n�zet t�pusa
    /// </summary>
    public partial class GameForm : Form
    {
        #region Fields

        private MinefieldGameModel _model;          // modell
        private IMinefieldFileAccess _fileAccess;   // adatel�r�s
        private TableLayoutPanel _grid;             // j�t�kt�ret megjelen�t� r�cs

        #endregion

        #region Assets

        // F�jlb�l import�lt k�pek
        private Image _easyMineImg;
        private Image _mediumMineImg;
        private Image _hardMineImg;
        private Image _playerImg;

        #endregion

        #region Constructor

        /// <summary>
        /// N�zet ablakot p�ld�nyos�t� konstruktor
        /// </summary>
        public GameForm()
        {
            InitializeComponent();

            _fileAccess = new MinefieldFileAccess();
            _model = new MinefieldGameModel(_fileAccess);

            _model.TimeAdvanced += new EventHandler<MinefieldEventArgs>(UpdateGameTime);
            _model.CellUpdate += new EventHandler<MinefieldCellUpdateEventArgs>(UpdateGridCell);
            _model.GameOver += new EventHandler<MinefieldEventArgs>(GameOver);

            WindowState = FormWindowState.Maximized;
            FormBorderStyle = FormBorderStyle.FixedSingle;

            _easyMineImg = Image.FromFile(@"Img\EasyMine.png");
            _mediumMineImg = Image.FromFile(@"Img\MediumMine.png");
            _hardMineImg = Image.FromFile(@"Img\HardMine.png");
            _playerImg = Image.FromFile(@"Img\Submarine.png");

            _grid = new TableLayoutPanel();
            SetupGrid();

            _pauseMenuStrip.Visible = true;
            _menuSaveGame.Enabled = false;
        }
        #endregion

        #region Menu even handlers
        /// <summary>
        /// �j j�t�k men�pont esem�nykez�je
        /// </summary>
        private void MenuNewGame_Click(object sender, EventArgs e)
        {
            SetupStartGame();
            _model.StartNewGame();
        }

        /// <summary>
        /// J�t�k ment�se men�pont esem�nykezel�je
        /// </summary>
        private void MenuSaveGame_Click(object sender, EventArgs e)
        {
            if (_model.IsGameRunning && _saveFileDialog.ShowDialog() == DialogResult.OK)
            {
                try
                {
                    _model.SaveGame(_saveFileDialog.FileName);
                }
                catch (MinefieldFileAccessException)
                {
                    MessageBox.Show("Incorrect path or access denied.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                }
            }
            PauseGame();
        }

        /// <summary>
        /// J�t�k bet�lt�se men�pont esem�nykezel�je
        /// </summary>
        private void MenuLoadGame_Click(object sender, EventArgs e)
        {
            if (_openFileDialog.ShowDialog() == DialogResult.OK)
            {
                try
                {
                    _model.LoadGame(_openFileDialog.FileName);
                    LoadTable();
                    _menuSaveGame.Enabled = true;
                }
                catch (MinefieldFileAccessException)
                {
                    MessageBox.Show("Incorrect path or file format.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                }
            }
            PauseGame();
        }
#endregion

        #region Form input event handlers

        /// <summary>
        /// Billenty�le�t�s esem�nykezel�je
        /// </summary>
        private void GameForm_KeyDown(Object sender, KeyEventArgs e)
        {
            switch (e.KeyCode)
            {
                case Keys.W:
                    _model.MovePlayer(Direction.Up);
                    return;
                case Keys.S:
                    _model.MovePlayer(Direction.Down);
                    return;
                case Keys.A:
                    _model.MovePlayer(Direction.Left);
                    return;
                case Keys.D:
                    _model.MovePlayer(Direction.Right);
                    return;
                case Keys.Space:
                    if (_model.IsGameRunning)
                    {
                        PauseGame();
                    }
                    return;
            }
        }

        /// <summary>
        /// J�t�kablak bez�r�s�nak esem�nykezel�je
        /// </summary>
        private void GameForm_FormClosing(object sender, FormClosingEventArgs e)
        {
            if (!_model.IsGamePaused) PauseGame();
            if (MessageBox.Show("Are you sure you want to quit?", "Exit Application", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.No)
            {
                e.Cancel = true;
            }
        }
        #endregion

        #region Model event handlers

        /// <summary>
        /// J�t�kid� halad�s�nak esem�nykezel�je
        /// </summary>
        private void UpdateGameTime(Object? sender, MinefieldEventArgs e)
        {
            statusLabel.Text = $"{e.Time.Minutes:00}:{e.Time.Seconds:00}";
        }

        /// <summary>
        /// J�t�k v�g�nek esem�nykezel�je
        /// </summary>
        private void GameOver(Object? sender, MinefieldEventArgs e)
        {
            Invoke(new Action(() =>
            {
                _grid.BackColor = Color.Gray;
                _pauseMenuStrip.Visible = true;
                _menuSaveGame.Enabled = false;
            }));
            MessageBox.Show($"Time survived: {(int)e.Time.TotalMinutes:0}:{e.Time.Seconds:00}.{e.Time.Milliseconds / 100:0}", "Game Over", MessageBoxButtons.OK, MessageBoxIcon.Information);
        }

        /// <summary>
        /// Cella v�ltoz�s�nak esem�nykezel�je
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        public void UpdateGridCell(Object? sender, MinefieldCellUpdateEventArgs e)
        {
            ModifyCell(e.X, e.Y);
        }

        #endregion

        #region Private methods

        /// <summary>
        /// J�t�k sz�neteltet�se vagy annak felold�sa
        /// </summary>
        private void PauseGame()
        {
            _model.TogglePaused();
            if (_model.IsGamePaused)
            {
                _grid.BackColor = Color.Gray;
                _pauseMenuStrip.Visible = true;
            }
            else
            {
                _grid.BackColor = Color.MidnightBlue;
                _pauseMenuStrip.Visible = false;
            }
        }

        /// <summary>
        /// N�zet el�k�sz�t�se �j j�t�k kezd�s�re
        /// </summary>
        private void SetupStartGame()
        {
            _grid.Controls.Clear();
            _pauseMenuStrip.Visible = false;
            _menuSaveGame.Enabled = true;
            _grid.BackColor = Color.MidnightBlue;
        }

        /// <summary>
        /// A teljes j�t�kt�r megjelen�t�se
        /// </summary>
        private void LoadTable()
        {
            for (int i = 0; i < _model._gridRows; i++)
            {
                for (int j = 0; j < _model._gridColumns; j++)
                {
                    ModifyCell(i, j);
                }
            }
        }

        /// <summary>
        /// Cella tartlam�nak be�ll�t�sa a modell j�t�ktere alapj�n
        /// </summary>
        /// <param name="x">X-koordin�ta (sor)</param>
        /// <param name="y">Y-koordin�ta (oszlop)</param>
        private void ModifyCell(int x, int y)
        {
            if (_model[x, y] is null)
            {
                Control? pb = _grid.GetControlFromPosition(y, x);
                if (pb != null)
                {
                    Invoke(new Action(() =>
                    {
                        _grid.Controls.Remove(pb);
                    }));
                }
            }
            else
            {
                PictureBox pb = new PictureBox
                {
                    Image = ImageOfEntity(_model[x, y]!)!,
                    SizeMode = PictureBoxSizeMode.Zoom
                };

                Invoke(new Action(() =>
                {
                    _grid.Controls.Add(pb, y, x);
                }));
            }
        }

        /// <summary>
        /// R�cs kezdeti be�ll�t�sainak elv�gz�se
        /// </summary>
        private void SetupGrid()
        {
            _grid.Dock = DockStyle.Fill;
            _grid.RowCount = _model._gridRows;
            _grid.ColumnCount = _model._gridColumns;
            _grid.RowStyles.Clear();
            _grid.ColumnStyles.Clear();
            for (int i = 0; i < _model._gridRows; i++)
            {
                _grid.RowStyles.Add(new RowStyle(SizeType.Percent, 1f / _model._gridRows));
            }
            for (int i = 0; i < _model._gridColumns; i++)
            {
                _grid.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 1f / _model._gridColumns));
            }
            _grid.BackColor = Color.Gray;
            Controls.Add(_grid);
        }

        /// <summary>
        /// Entit�shoz tartoz� k�p megad�sa
        /// </summary>
        /// <param name="en">Adott entit�s</param>
        /// <returns>Entit�snak megfelel� Image objektum, vagy null, ha nincs ilyen</returns>
        private Image? ImageOfEntity(Entity en)
        {
            if (en is EasyMine)
            {
                return _easyMineImg;
            }
            else if (en is MediumMine)
            {
                return _mediumMineImg;
            }
            else if (en is HardMine)
            {
                return _hardMineImg;
            }
            else if (en is Player)
            {
                return _playerImg;
            }
            return null;
        }
        #endregion
    }
}
