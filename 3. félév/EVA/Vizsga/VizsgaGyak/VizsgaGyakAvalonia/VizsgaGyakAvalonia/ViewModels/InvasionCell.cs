using Avalonia.Media;
using System;

namespace VizsgaGyakAvalonia.ViewModels
{
    public class InvasionCell : ViewModelBase
    {
        private readonly int _x;
        private readonly int _y;
        private string _text = null!;

        public InvasionCell(int x, int y)
        {
            _x = x;
            _y = y;
        }

        public int X => _x;
        public int Y => _y;
        public Tuple<int, int> Coordniate
        {
            get => new(_x, _y);
            init
            {
                _x = value.Item1;
                _y = value.Item2;
            }
        }

        public string Text
        {
            get => _text;
            set
            {
                _text = value;
                OnPropertyChanged();
            }
        }
    }
}
