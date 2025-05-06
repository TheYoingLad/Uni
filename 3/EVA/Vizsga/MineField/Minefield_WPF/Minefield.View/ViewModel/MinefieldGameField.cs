using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Media.Imaging;

namespace Minefield.WPF.ViewModel
{
    /// <summary>
    /// Aknamező játéktér mező típusa
    /// </summary>
    public class MinefieldGameField : MinefieldViewModelBase
    {
        private int _x, _y;
        private bool _notEmpty;
        private string _entity;

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
            set {
                _notEmpty = value;
                OnPropertyChanged();
            }
        }
        /// <summary>
        /// Mező tartalma
        /// </summary>
        public string Entity
        {
            get => _entity;
            set
            {
                _entity = value;
                OnPropertyChanged();
            }
        }
        /// <summary>
        /// Mezőt példányosító konstruktor
        /// </summary>
        public MinefieldGameField(int x, int y)
        {
            _x = x;
            _y = y;
            _notEmpty = false;
            _entity = "";
        }
    }
}
