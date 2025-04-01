using Minefield.Model;

namespace Minefield.Persistence
{
    /// <summary>
    /// Aknamező játék fájlkezelő interfésze
    /// </summary>
    public interface IMinefieldFileAccess
    {
        /// <summary>
        /// Fájl mentése
        /// </summary>
        /// <param name="path">Fájl elérési útvonala</param>
        /// <param name="time">Játékidő a mentéskor</param>
        /// <param name="e">Játékteret reprezentáló Entitiy mátrix</param>
        public void SaveFile(string path, TimeSpan time, Entity?[,] e);

        /// <summary>
        /// Fájl betöltése
        /// </summary>
        /// <param name="path">Fájl elérési útvonala</param>
        /// <returns>(Játékteret reprezentáló Entitiy mátrix, Játékos objektum, Játékidő) rendezett hármas</returns>
        public (Entity?[,], Player, TimeSpan) LoadFile(string path);
    }
}
