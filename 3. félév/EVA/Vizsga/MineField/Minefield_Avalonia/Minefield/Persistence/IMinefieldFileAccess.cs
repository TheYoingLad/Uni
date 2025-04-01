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
        public Task SaveFileAsync(string path, TimeSpan time, Entity?[,] e);

        /// <summary>
        /// Fájl mentése
        /// </summary>
        /// <param name="stream">Adatfolyam</param>
        /// <param name="time">Játékidő a mentéskor</param>
        /// <param name="e">Játékteret reprezentáló Entitiy mátrix</param>
        public Task SaveFileAsync(Stream stream, TimeSpan time, Entity?[,] e);

        /// <summary>
        /// Fájl betöltése
        /// </summary>
        /// <param name="path">Fájl elérési útvonala</param>
        /// <returns>(Játékteret reprezentáló Entitiy mátrix, Játékos objektum, Játékidő) rendezett hármas</returns>
        public Task<(Entity?[,], Player, TimeSpan)> LoadFileAsync(string path);

        /// <summary>
        /// Fájl betöltése
        /// </summary>
        /// <param name="stream">Adatfolyam</param>
        /// <returns>(Játékteret reprezentáló Entitiy mátrix, Játékos objektum, Játékidő) rendezett hármas</returns>
        public Task<(Entity?[,], Player, TimeSpan)> LoadFileAsync(Stream stream);
    }
}
