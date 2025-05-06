using System.Windows.Media;

namespace VizsgaGyakWPF.ViewModel
{
    /// <summary>
    /// A cell for the Laser Pigs game
    /// </summary>
    public class AttackCell : ViewModelBase
    {
        #region Fields
        private int _x;
        private int _y;
        private string _text = "";
        private SolidColorBrush? _backColour;
        #endregion

        #region Properties
        public Tuple<int, int> Coordinate
        {
            get => new(_x, _y);
            set
            {
                _x = value.Item1;
                _y = value.Item2;
            }
        }
        public int GetX => _x;
        public int GetY => _y;
        public string Text
        {
            get => _text;
            set
            {
                if (_text != value)
                {
                    _text = value;
                    OnPropertyChanged();
                }
            }
        }
        public SolidColorBrush BackColour
        {
            get => _backColour ?? Brushes.White;
            set
            {
                if (_backColour != value)
                {
                    _backColour = value;
                    OnPropertyChanged();
                }
            }
        }
        #endregion
    }
}
