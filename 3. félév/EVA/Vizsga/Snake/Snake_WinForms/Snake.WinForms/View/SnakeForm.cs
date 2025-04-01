using Snake.Model;
using Snake.Persistence;

namespace Snake.View
{
    public partial class Form : System.Windows.Forms.Form
    {
        #region Fields

        private GameModel? model = null;
        private List<List<PictureBox>> tiles = new();
        private int resolution = -1;

        #endregion

        #region Constructor(s)

        public Form()
        {
            InitializeComponent();

            // catching all key events even when other UI element is in focus
            this.KeyPreview = true;
        }

        #endregion

        #region Utilities

        private Color TileColor(Tile tile)
        {
            switch (tile.type)
            {
                case TileType.Empty:
                    return Color.White;
                case TileType.Snake:
                    return Color.FromArgb(108, 187, 60);
                case TileType.Wall:
                    return Color.Firebrick;
                case TileType.Egg:
                    return Color.FromArgb(192, 187, 171);
                case TileType.SnakeHead:
                    return Color.FromArgb(86, 150, 48);
                default:
                    return Color.White;
            }
        }

        private void ParameterizeModel()
        {
            if (model == null) return;

            model.ResetTiles += UpdateAllTiles;
            model.UpdateTile += UpdateSingleTile;
            model.StopEvent += StopGame;
        }

        #endregion

        #region Model Event Methods

        private void UpdateAllTiles(object? sender, EventArgs? e)
        {
            if (model == null) return;

            // Reconstruct PictureBox Grid if needed (resolution has changed)
            if (this.resolution != model.Resolution)
            {
                this.resolution = model.Resolution;

                // Delete previous Tiles from Form Control and from List
                foreach (var col_tiles in tiles)
                {
                    foreach (var tile in col_tiles)
                    {
                        this.Controls.Remove(tile);
                    }
                }
                tiles.Clear();

                // Generating Tiles to cover canvas
                int tileWidth = (int)Math.Floor(this.ClientRectangle.Width / (float)resolution);
                int tileHeight = (int)Math.Floor(this.ClientRectangle.Height / (float)resolution);
                int tileSize = Math.Min(tileWidth, tileHeight);

                int shiftX = (this.ClientRectangle.Width - tileSize * resolution) / 2;
                int shiftY = (this.ClientRectangle.Height - tileSize * resolution) / 2;

                for (int x = 0; x < resolution; x++)
                {
                    tiles.Add(new List<PictureBox>());
                    for (int y = 0; y < resolution; y++)
                    {
                        PictureBox pic = new PictureBox();
                        int index = x + resolution * y;

                        // properties of PictureBox
                        pic.Location = new Point(shiftX + x * tileSize, shiftY + y * tileSize);
                        pic.Name = $"tile{index}";
                        pic.Size = new Size(tileSize, tileSize);
                        pic.TabIndex = index;
                        pic.TabStop = false;

                        this.Controls.Add(pic);
                        tiles[x].Add(pic);
                    }
                }
            }

            // Color All the Tiles Accordingly
            for (int x = 0; x < resolution; x++)
            {
                for (int y = 0; y < resolution; y++)
                {
                    PictureBox pic = tiles[x][y];
                    pic.BackColor = model.Tiles == null ? Color.Pink : TileColor(model.Tiles[x][y]);
                }
            }
        }

        private void UpdateSingleTile(object? sender, TileArgs e)
        {
            if (model == null) return;

            Coordinate? pos = e.coord;
            if (pos == null) return;

            if (pos.X < 0 || pos.X >= tiles.Count || pos.Y < 0 || pos.Y >= tiles[pos.X].Count) return;

            this.tiles[pos.X][pos.Y].BackColor = TileColor(model.Tiles![pos.X][pos.Y]);
        }

        private void StopGame(object? sender, StopGameArgs e)
        {
            // Invoke needed to give control to main thread avoiding cross thread operation on UI
            Invoke(new Action(() =>
            {
                bool ingame = e.inGame;
                if (!ingame)
                {
                    StartBtn.Text = "Play";

                    ScoreLabel.Text = string.Format("Score: {0:000}", model?.Score ?? 0);
                    GameOverLabel.ForeColor = Color.Red;
                    GameOverLabel.Text = "Game Over";
                    ScoreLabel.Visible = true;
                }
                else
                {
                    StartBtn.Text = "Resume";
                    GameOverLabel.ForeColor = Color.DarkOrange;
                    GameOverLabel.Text = "Paused";
                    ScoreLabel.Visible = false;
                }

                MenuBox.Visible = true;
            }));
        }

        #endregion

        #region View Event Methods

        private void Form_KeyDown(object sender, KeyEventArgs e)
        {
            if (model == null) return;

            if (e.KeyCode == Keys.Left || e.KeyCode == Keys.A)
            {
                model.TurnLeft();
            }
            else if (e.KeyCode == Keys.Right || e.KeyCode == Keys.D)
            {
                model.TurnRight();
            }
            else if (e.KeyCode == Keys.Escape)
            {
                if (!MenuBox.Visible) model.StopGame(gameOver: false);
                else if (model.Ingame)
                {
                    MenuBox.Visible = false;
                    model.StartGame(newGame: !model.Ingame);
                }
            }
        }

        private void StartBtn_Click(object sender, EventArgs e)
        {
            if (model == null)
            {
                // If no model is present then ask whether to play on an empty map is wanted

                DialogResult result = MessageBox.Show("No map has been loaded in. Are you sure to play on an empty map?",
                    "No map loaded", MessageBoxButtons.YesNo, MessageBoxIcon.Warning);

                if (result == DialogResult.Yes)
                {
                    model?.Dispose(); // dispose potential previous model (model should be null here)
                    model = new GameModel();
                    ParameterizeModel();

                    MenuBox.Visible = false;
                    model.StartGame(newGame: true);
                }
            }
            else
            {
                if (!model.Ingame) model.ResetGame();
                
                MenuBox.Visible = false;
                model.StartGame(newGame: !model.Ingame);
            }
        }

        private void LoadMapBtn_Click(object sender, EventArgs e)
        {
            using (OpenFileDialog fileDialog = new OpenFileDialog())
            {
                fileDialog.InitialDirectory = ".";
                fileDialog.Filter = "save files (*.save)|*.save|All files (*.*)|*.*";
                fileDialog.RestoreDirectory = true;

                if (fileDialog.ShowDialog() == DialogResult.OK)
                {
                    try
                    {
                        IDataAccess dataAccess = new SaveFileDataAccess(fileDialog.FileName);
                        model?.Dispose(); // dispose potential previous model
                        model = new GameModel(dataAccess);
                        ParameterizeModel();

                        loadedLabel.Visible = true;
                        GameOverLabel.Text = "";
                        ScoreLabel.Visible = false;

                        StartBtn.Text = "Play";
                    }
                    catch (Exception ex)
                    {
                        MessageBox.Show($"Error loading map in: {ex.Message}");
                    }
                }
            }
        }

        private void ExitBtn_Click(object sender, EventArgs e)
        {
            Application.Exit();
        }

        #endregion
    }
}