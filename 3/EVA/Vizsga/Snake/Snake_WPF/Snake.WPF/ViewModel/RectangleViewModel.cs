using Snake.Model;

namespace Snake.WPF.ViewModel
{
    public class RectangleViewModel : ViewModelBase
    {
        // private Fields
        private TileType type = TileType.Empty;

        // public Properties
        public int Row { get; set; }
        public int Column { get; set; }
        public TileType Type
        {
            get => type;
            set
            {
                type = value;
                OnPropertyChanged();
            }
        }
        public double Width { get; set; }
        public double Height { get; set; }
    }
}
