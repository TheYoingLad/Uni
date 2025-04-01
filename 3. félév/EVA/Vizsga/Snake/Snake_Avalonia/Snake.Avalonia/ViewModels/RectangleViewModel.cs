using Snake.Model;
using CommunityToolkit.Mvvm.Input;
using CommunityToolkit.Mvvm.ComponentModel;

namespace Snake.Avalonia.ViewModels
{
    public class RectangleViewModel : ObservableObject
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

                OnPropertyChanged(nameof(IsEmpty));
                OnPropertyChanged(nameof(IsSnake));
                OnPropertyChanged(nameof(IsWall));
                OnPropertyChanged(nameof(IsEgg));
                OnPropertyChanged(nameof(IsSnakeHead));
            }
        }
        public double Width { get; set; }
        public double Height { get; set; }

        public bool IsEmpty
        {
            get => Type == TileType.Empty;
            set => OnPropertyChanged();
        }

        public bool IsSnake
        {
            get => Type == TileType.Snake;
            set => OnPropertyChanged();
        }

        public bool IsWall
        {
            get => Type == TileType.Wall;
            set => OnPropertyChanged();
        }

        public bool IsEgg
        {
            get => Type == TileType.Egg;
            set => OnPropertyChanged();
        }

        public bool IsSnakeHead
        {
            get => Type == TileType.SnakeHead;
            set => OnPropertyChanged();
        }
    }
}
