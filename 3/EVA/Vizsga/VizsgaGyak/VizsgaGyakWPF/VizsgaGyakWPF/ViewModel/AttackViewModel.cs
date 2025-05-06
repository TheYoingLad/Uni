using System.Collections.ObjectModel;
using System.Windows;
using System.Windows.Media;
using VizsgaGyakWPF.Model;

namespace VizsgaGyakWPF.ViewModel
{
    public class AttackViewModel : ViewModelBase
    {
        private AttackModel _model;

        public AttackViewModel(AttackModel model)
        {
            _model = model;
            _model.PlayerStepped += Model_PlayerStep;

            StepCommand = new DelegateCommand(param =>
            {
                if (param is Tuple<int, int> coord)
                {
                    try
                    {
                        _model.Step((coord.Item1, coord.Item2));
                    }
                    catch (Exception e)
                    {
                        MessageBox.Show(e.Message, "Error", MessageBoxButton.OK, MessageBoxImage.Error);
                    }
                }
            });
            NewGameCommand = new DelegateCommand(param => { if (param is string s) OnNewGame(int.Parse(s)); });

            Map = new ObservableCollection<AttackCell>();
            GenerateMap();
        }

        public int FontSize => 600 / Size / 4;
        public int Size => _model.Size;
        public int CurrentPlayer => _model.CurrentPlayer;
        public int CurrentPiece => _model.CurrentPiece;

        public DelegateCommand StepCommand { get; private set; }
        public DelegateCommand NewGameCommand { get; private set; }
        public ObservableCollection<AttackCell> Map { get; set; }

        public event EventHandler<int>? NewGame;


        private void Model_PlayerStep(object? sender, StepEventArg e)
        {
            (int X, int Y) fromCoord = e.GetFromCoord;
            (int X, int Y) toCoord = e.GetToCoord;

            AttackCell fromCell = Map.Single(c => c.GetX == fromCoord.X && c.GetY == fromCoord.Y);
            AttackCell toCell = Map.Single(c => c.GetX == toCoord.X && c.GetY == toCoord.Y);

            string playerText = fromCell.Text;
            SolidColorBrush playerColour = fromCell.BackColour;

            fromCell.Text = "";

            toCell.Text = playerText;
            toCell.BackColour = playerColour;

            OnPropertyChanged(nameof(CurrentPlayer));
            OnPropertyChanged(nameof(CurrentPiece));
        }

        private void GenerateMap()
        {
            Map.Clear();
            for (int i = 0; i < Size; i++)
            {
                for (int j = 0; j < Size; j++)
                {
                    Map.Add(new AttackCell
                    {
                        Coordinate = Tuple.Create(i, j),
                        Text = _model[i, j]
                    });
                }
            }
        }

        private void OnNewGame(int n)
        {
            NewGame?.Invoke(this, n);
        }
    }
}
