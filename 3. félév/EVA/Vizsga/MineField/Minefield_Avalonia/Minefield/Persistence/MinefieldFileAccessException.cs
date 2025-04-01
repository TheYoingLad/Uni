namespace Minefield.Persistence
{
    /// <summary>
    /// Aknamező játék fájlelérés kivétel típusa
    /// </summary>
    public class MinefieldFileAccessException : IOException
    { 
        /// <summary>
        /// Fájlelérés kivételt példányosító konstruktor
        /// </summary>
        public MinefieldFileAccessException() : base() { }
    }
}
