using System;
using System.Collections.ObjectModel;
using System.Linq;
using VizsgaGyakAvalonia.Model;

namespace VizsgaGyakAvalonia.ViewModels
{
    /// <summary>
    /// The viewmodel for the Laser Pigs game
    /// </summary>
    public class InvasionViewModel : ViewModelBase
    {
        private InvasionModel _model;
        public InvasionViewModel(InvasionModel model)
        {
            _model = model;
            _model.Step += Model_Step;
            _model.AdvanceTime += Model_AdvanceTime;

            Map = new ObservableCollection<InvasionCell>();
            for (int i = 0; i < Rows; i++) for (int j = 0; j < Cols; j++) Map.Add(new InvasionCell(i, j));
            SetupMap();

            PauseResume = new DelegateCommand(_ =>
            {
                if (_model.IsPaused) _model.TimerResume();
                else _model.TimerPause();
                OnPropertyChanged(nameof(PauseButtonText));
            });
        }
        public InvasionViewModel() : this(new InvasionModel()) { }

        public ObservableCollection<InvasionCell> Map { get; set; }

        public DelegateCommand PauseResume { get; set; }

        public int Rows => 10;
        public int Cols => 20;
        public string GameTime => _model.GameTime.ToString(@"hh\:mm\:ss");
        public string PauseButtonText => _model.IsPaused ? "Resume" : "Pause";
        public string HP => _model.HP.ToString();
        public string Placeable => _model.Placeable.ToString();


        private void SetupMap()
        {
            for (int i = 0; i < Rows; i++)
            {
                for (int j = 0; j < Cols; j++)
                {
                    InvasionCell cell = Map.Single(c => c.X == i && c.Y == j);
                    cell.Text = _model[i, j];
                }
            }
        }


        private void Model_Step(object? sender, StepArgs e)
        {
            InvasionCell fromCell = Map.Single(c => c.X == e.X && c.Y == e.Y);

            if(e.wasKilled == null)
            {
                InvasionCell toCell = Map.Single(c => c.X == e.X && c.Y == e.Y + 1);
                toCell.Text = fromCell.Text;
            }
            fromCell.Text = "";
            OnPropertyChanged(nameof(Placeable));
            OnPropertyChanged(nameof(HP));
        }

        private void Model_AdvanceTime(object? sender, EventArgs e)
        {
            OnPropertyChanged(nameof(GameTime));
        }
    }
}
