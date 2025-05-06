namespace Grid
{
    public partial class GridForm : Form
    {
        private GridButton[,]? _buttons;
        private Random _random;
        public GridForm()
        {
            InitializeComponent();

            _random = new Random();
        }

        void ButtonSize_Click(object sender, EventArgs e)
        {
            if (_buttons != null) foreach (GridButton b in _buttons) _tableLayoutGrid.Controls.Remove(b);

            _buttons = new GridButton[Convert.ToInt32(_numericRows.Value), Convert.ToInt32(_numericColumns.Value)];

            _tableLayoutGrid.RowCount = Convert.ToInt32(_numericRows.Value);
            _tableLayoutGrid.ColumnCount = Convert.ToInt32(_numericColumns.Value);

            for (int i = 0; i < _numericRows.Value; i++)
            {
                for (int j = 0; j < _numericColumns.Value; j++)
                {
                    _buttons[i, j] = new GridButton(i, j);
                    _buttons[i, j].BackColor = Color.White;
                    _buttons[i, j].Dock = DockStyle.Fill;
                    _buttons[i, j].Click += new EventHandler(GridButton_Click);

                    _tableLayoutGrid.Controls.Add(_buttons[i, j], j, i);
                }
            }

            _tableLayoutGrid.RowStyles.Clear();
            _tableLayoutGrid.ColumnStyles.Clear();

            for (int i = 0; i < _numericRows.Value; i++) _tableLayoutGrid.RowStyles.Add(new RowStyle(SizeType.Percent, 1 / Convert.ToSingle(_numericRows.Value)));
            for (int i = 0; i < _numericColumns.Value; i++) _tableLayoutGrid.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 1 / Convert.ToSingle(_numericColumns.Value)));

        }
        void GridButton_Click(object? sender, EventArgs e)
        {
            if (sender is GridButton b)
            {
                int x = b.GridX;
                int y = b.GridY;

                Color c = Color.FromArgb(_random.Next(256), _random.Next(256), _random.Next(256));

                for (int i = 0; i < _buttons!.GetLength(0); i++) _buttons[i, y].BackColor = c;
                for (int j = 0; j < _buttons!.GetLength(1); j++) _buttons[x, j].BackColor = c;
            }
        }

    }
}
