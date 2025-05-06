using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Input;

namespace Minefield.WPF.ViewModel
{
    /// <summary>
    /// Parancs típus
    /// </summary>
    public class MinefieldDelegateCommand : ICommand
    {
        private readonly Action<object?> _execute;          // tevékenység
        private readonly Predicate<object?>? _canExecute;   // predikátum

        /// <summary>
        /// Végrehajthatóság változása esemény
        /// </summary>
        public event EventHandler? CanExecuteChanged;
        
        /// <summary>
        /// Parancsot példányosító konstruktor
        /// </summary>
        /// <param name="execute">Tevékenység</param>
        /// <param name="canExecute">Predikátum</param>
        public MinefieldDelegateCommand(Action<object?> execute, Predicate<object?>? canExecute = null)
        {
            if (execute == null) throw new ArgumentNullException(nameof(execute));
            _execute = execute;
            _canExecute = canExecute;
        }

        /// <summary>
        /// Parancs végrehajthatóságának ellenőrése
        /// </summary>
        /// <param name="parameter">Tevékenység paramétere</param>
        /// <returns>Igaz, ha végrehajtható</returns>
        public bool CanExecute(object? parameter)
        {
            if (_canExecute == null)
                return true;
            else
                return _canExecute(parameter);
        }

        /// <summary>
        /// Parancs tevékenységének végrehajtása
        /// </summary>
        /// <param name="parameter">Tevékenység paramétere</param>
        public void Execute(object? parameter)
        {
            if (!CanExecute(parameter))
                throw new InvalidOperationException();
            else
                _execute(parameter);
        }

        /// <summary>
        /// Végrehajthatóság változása esemény kiváltása
        /// </summary>
        public void RaiseCanExecuteChanged() => CanExecuteChanged?.Invoke(this, EventArgs.Empty);

    }
}
