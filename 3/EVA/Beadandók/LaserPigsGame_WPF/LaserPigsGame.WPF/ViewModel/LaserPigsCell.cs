using LaserPigs.Persistence;
using System.Windows.Media;

namespace LaserPigsGame.ViewModel
{
    /// <summary>
    /// A cell for the Laser Pigs game
    /// </summary>
    public class LaserPigsCell : ViewModelBase
    {
        #region Fields
        private Coordinate _coordinate = new Coordinate(0, 0);
        private string _text = "";
        private SolidColorBrush? _backColour;
        private SolidColorBrush? _textColour;
        #endregion

        #region Properties
        public Coordinate Coordinate
        {
            get => _coordinate;
            set => _coordinate = value;
        }
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
            get => _backColour ?? Brushes.LightGreen;
            set
            {
                if (_backColour != value)
                {
                    _backColour = value;
                    OnPropertyChanged();
                }
            }
        }
        public SolidColorBrush TextColour
        {
            get => _textColour ?? Brushes.Black;
            set
            {
                if (_textColour != value)
                {
                    _textColour = value;
                    OnPropertyChanged();
                }
            }
        }
        #endregion
    }
}
