namespace Minefield.Model
{
    /// <summary>
    /// Aknamező játék cellaváltozás esemény argumentumának típusa
    /// </summary>
    public class MinefieldCellUpdateEventArgs : EventArgs
    {
        private int _x;
        private int _y;

        /// <summary>
        /// Cella X-koordinátája (sor)
        /// </summary>
        public int X { get { return _x; } }

        /// <summary>
        /// Cella Y-koordinátája (oszlop)
        /// </summary>
        public int Y { get { return _y; } }

        /// <summary>
        /// Cellaváltozás esemény argumentumát példányosító konstruktor
        /// </summary>
        /// <param name="x">Cella X-koordinátája (sor)</param>
        /// <param name="y">Cella Y-koordinátája (oszlop)</param>
        public MinefieldCellUpdateEventArgs(int x, int y)
        {
            _x = x;
            _y = y;
        }
    }
}
