using Minefield.Persistence;

namespace Minefield.Avalonia.ViewModels
{
    /// <summary>
    /// Aknamező játéktér mező típusa
    /// </summary>
    public class MinefieldGameField : ViewModelBase
    {
        private int _x, _y;
        private bool _notEmpty;
        private bool _isEasyMine, _isMediumMine, _isHardMine, _isPlayer;

        /// <summary>
        /// mező X-koordinátája (sor)
        /// </summary>
        public int X { get => _x; }

        /// <summary>
        /// mező Y-koordinátája (oszlop)
        /// </summary>
        public int Y { get => _y; }

        /// <summary>
        /// Mező üressége
        /// </summary>
        public bool NotEmpty
        {
            get => _notEmpty;
            set
            {
                _notEmpty = value;
                OnPropertyChanged();
            }
        }

        /// <summary>
        /// Player entitás van-e a mezőn
        /// </summary>
        public bool IsPlayer { get => _isPlayer; set { _isPlayer = value; OnPropertyChanged(); } }

        /// <summary>
        /// Player entitás van-e a mezőn
        /// </summary>
        public bool IsEasyMine { get => _isEasyMine; set { _isEasyMine= value; OnPropertyChanged(); } }

        /// <summary>
        /// Player entitás van-e a mezőn
        /// </summary>
        public bool IsMediumMine { get => _isMediumMine; set { _isMediumMine = value; OnPropertyChanged(); } }

        /// <summary>
        /// Player entitás van-e a mezőn
        /// </summary>
        public bool IsHardMine { get => _isHardMine; set { _isHardMine = value; OnPropertyChanged(); } }

        /// <summary>
        /// Mező tartalma
        /// </summary>
        public Entity? Entity
        {
            set
            {
                IsEasyMine = value is EasyMine;
                IsMediumMine = value is MediumMine;
                IsHardMine = value is HardMine;
                IsPlayer = value is Player;
            }
        }
        /// <summary>
        /// Mezőt példányosító konstruktor
        /// </summary>
        public MinefieldGameField(int x, int y)
        {
            _x = x;
            _y = y;
            _notEmpty = _isEasyMine = _isMediumMine = _isHardMine = _isPlayer = false;
        }
    }
}
