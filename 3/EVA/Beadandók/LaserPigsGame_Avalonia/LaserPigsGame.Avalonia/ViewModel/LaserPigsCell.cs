using Avalonia.Media;
using LaserPigs.Persistence;

namespace LaserPigsGame.Avalonia.ViewModel
{
    /// <summary>
    /// A cell for the Laser Pigs game
    /// </summary>
    public class LaserPigsCell : ViewModelBase
    {
        #region Fields
        private Coordinate _coordinate = new Coordinate(0, 0);
        private string _text = "";
        private IImmutableBrush? _backColour;
        private IImmutableBrush? _textColour;
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
        public IImmutableBrush BackColour
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
        public IImmutableBrush TextColour
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
