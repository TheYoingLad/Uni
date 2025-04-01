using System.Windows.Input;

namespace Snake.WPF.ViewModel
{
    public class DelegateCommand : ICommand
    {
        private readonly Action<Object?> _execute;
        private readonly Func<Object?, Boolean>? _canExecute; // predicate of executing the action

        /// <summary>
        /// Generate Command
        /// </summary>
        /// <param name="execute">Executable action</param>
        public DelegateCommand(Action<Object?> execute) : this(null, execute) { }

        /// <summary>
        /// Generate Command
        /// </summary>
        /// <param name="canExecute">Predicate of execution</param>
        /// <param name="execute">Executable action</param>
        /// <exception cref="ArgumentNullException"></exception>
        public DelegateCommand(Func<Object?, Boolean>? canExecute, Action<Object?> execute)
        {
            if (execute == null) throw new ArgumentNullException(nameof(execute));

            _execute = execute;
            _canExecute = canExecute;
        }

        /// <summary>
        /// Event of executable state change
        /// </summary>
        public event EventHandler? CanExecuteChanged;

        /// <summary>
        /// Check executability
        /// </summary>
        /// <param name="parameter">Parameter of action</param>
        public Boolean CanExecute(Object? parameter)
        {
            return _canExecute == null ? true : _canExecute(parameter);
        }

        /// <summary>
        /// Execute action
        /// </summary>
        /// <param name="parameter">Parameter of action</param>
        /// <exception cref="InvalidOperationException"></exception>
        public void Execute(Object? parameter)
        {
            if (!CanExecute(parameter)) throw new InvalidOperationException("Command execution is disabled.");

            _execute(parameter);
        }

        /// <summary>
        /// Invoke CanExecuteChanged
        /// </summary>
        public void RaiseCanExecuteChanged()
        {
            if (CanExecuteChanged != null) CanExecuteChanged(this, EventArgs.Empty);
        }
    }
}
