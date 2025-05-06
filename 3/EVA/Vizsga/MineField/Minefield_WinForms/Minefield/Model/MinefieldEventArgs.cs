namespace Minefield.Model
{
    /// <summary>
    /// Aknamező játék esemény argumentum típusa
    /// </summary>
    public class MinefieldEventArgs : EventArgs
    {
        private TimeSpan _time;

        /// <summary>
        /// Játékidő
        /// </summary>
        public TimeSpan Time { get { return _time; } }

        /// <summary>
        /// Esemény argumentumot példányosító konstruktor
        /// </summary>
        /// <param name="time">Játékidő</param>
        public MinefieldEventArgs(TimeSpan time)
        {
            _time = time;
        }
    }
}
