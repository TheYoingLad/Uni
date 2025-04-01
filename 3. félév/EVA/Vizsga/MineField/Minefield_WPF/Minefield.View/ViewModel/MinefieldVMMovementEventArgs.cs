using Minefield.Model;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Minefield.WPF.ViewModel
{
    /// <summary>
    /// Mozgás vezérlés argumentumának típusa
    /// </summary>
    public class MinefieldVMMovementEventArgs : EventArgs
    {
        private Direction _dir;

        /// <summary>
        /// Mozgás iránya
        /// </summary>
        public Direction Dir { get { return _dir; } }

        /// <summary>
        /// Mozgás vezérlés argumentumot példányosító konstruktor
        /// </summary>
        /// <param name="dir">Mozgás iránya</param>
        public MinefieldVMMovementEventArgs(Direction dir)
        {
            _dir = dir;
        }
    }
}
