using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;

namespace Minefield.WPF.ViewModel
{
    /// <summary>
    /// Nézetmodell ősosztály
    /// </summary>
    public class MinefieldViewModelBase : INotifyPropertyChanged
    {
        /// <summary>
        /// Tulajdonság változása esemény
        /// </summary>
        public event PropertyChangedEventHandler? PropertyChanged;

        /// <summary>
        /// Nézetmodell ősosztályt példányosító konstruktor
        /// </summary>
        protected MinefieldViewModelBase() { }

        /// <summary>
        /// Tulajdonság változás esemény kiváltása ellenőrzéssel
        /// </summary>
        /// <param name="propertyName">Megváltozott tuladjonság neve</param>
        protected virtual void OnPropertyChanged([CallerMemberName] string? propertyName = null)
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }
    }
}
