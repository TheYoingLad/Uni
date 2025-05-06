using System.Buffers.Text;
using System.Windows.Forms;
using VizsgaGyakWinForms.Model;
using VizsgaGyakWinForms.Persistence;

namespace VizsgaGyakWinForms.View
{
    public partial class BlockForm : Form
    {
        private Button[,] _map = null!;
        private Button[,] _block = null!;
        private BlockModel _model;

        public BlockForm()
        {
            InitializeComponent();
            label1.Visible = false;

            _model = new BlockModel(new FileManager());
            _model.CellChanged += Model_CellChanged;
            _model.RowOrColChanged += Model_RowOrColChanged;
            _model.PointsAndBlockChanged += Model_PointsAndBlockChanged;

            GenerateMap();
            SetupBlock(0);

            _labelPoints.Text = "0";
        }

        private void GenerateMap()
        {
            int baseX = menuStrip1.ClientSize.Height;
            int baseY = 0;

            _map = new Button[4, 4];
            _block = new Button[2, 2];

            for (int i = 0; i < 4; i++)
            {
                for (int j = 0; j < 4; j++)
                {
                    _map[i, j] = new Button();
                    _map[i, j].Location = new Point(baseY + 20 + 150 * j, baseX + 20 + 150 * i);
                    _map[i, j].Size = new Size(150, 150);
                    _map[i, j].BackColor = Color.White;
                    _map[i, j].Name = i.ToString() + ',' + j.ToString();
                    _map[i, j].Click += MapButton_Click;

                    Controls.Add(_map[i, j]);
                }
            }

            baseY = 800;

            for (int i = 0; i < 2; i++)
            {
                for (int j = 0; j < 2; j++)
                {
                    _block[i, j] = new Button();
                    _block[i, j].Location = new Point(baseY + 20 + 150 * j, baseX + 20 + 150 * i);
                    _block[i, j].Size = new Size(150, 150);
                    _block[i, j].BackColor = Color.White;

                    Controls.Add(_block[i, j]);
                }
            }
        }

        private void Model_CellChanged(object? sender, CellEventArgs e)
        {
            (int x, int y) = e.GetCoordinate;
            _map[x, y].BackColor = Color.Blue;
        }
        private void Model_RowOrColChanged(object? sender, RowOrColEventArgs e)
        {
            int x = e.GetCoordinate;
            if (e.GetIsRow) for (int j = 0; j < 4; j++) _map[x, j].BackColor = Color.White;
            else for (int i = 0; i < 4; i++) _map[i, x].BackColor = Color.White;
        }
        private void Model_PointsAndBlockChanged(object? sender, (int points, int blockId) e)
        {
            _labelPoints.Text = e.points.ToString();
            ResetBlock();
            SetupBlock(e.blockId);
        }


        /*private void Menu_SaveGame_Click(object sender, EventArgs e)
        {
            using (SaveFileDialog saveFileDialog = new SaveFileDialog())
            {
                saveFileDialog.InitialDirectory = Application.StartupPath;
                saveFileDialog.RestoreDirectory = true;
                saveFileDialog.Filter = "Block files (*.block)|*.block";

                if (saveFileDialog.ShowDialog() == DialogResult.OK)
                {
                    try
                    {
                        _model.Save(saveFileDialog.FileName);
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
                openFileDialog.Filter = "Block files (*.block)|*.block";

                if (openFileDialog.ShowDialog() == DialogResult.OK)
                {
                    try
                    {
                        _model.Load(openFileDialog.FileName);
                    }
                    catch (Exception ex)
                    {
                        MessageBox.Show("An error occured:\n" + ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                        MessageBox.Show("File could not be loaded, restarting game", "Restart", MessageBoxButtons.OK, MessageBoxIcon.Information); //redo

                        _model.NewGame();
                    }

                    GenerateMap();

                    SetupMenus();
                    SetupMap();

                    SetupHealthBars();
                    RefreshHp(true, _model.GetP1Hp);
                    RefreshHp(false, _model.GetP2Hp);
                }
            }
        }*/

        private void MapButton_Click(object? sender, EventArgs e)
        {
            if (sender is Button b)
            {
                string[] coords = b.Name.Split(',');
                int x = int.Parse(coords[0]);
                int y = int.Parse(coords[1]);

                try
                {
                    _model.Place(x, y);
                }
                catch (Exception ex)
                {
                    MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                }
            }
        }

        private void ResetBlock()
        {
            for (int i = 0; i < 2; i++) for (int j = 0; j < 2; j++) _block[i, j].BackColor = Color.White;
        }
        private void SetupBlock(int blockId)
        {
            ResetBlock();
            List<(int, int)> coords = Block.GetBlockCoords(blockId);
            foreach ((int x, int y) coord in coords)
            {
                _block[coord.x, coord.y].BackColor = Color.Blue;
            }
        }
    }
}
