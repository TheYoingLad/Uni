namespace DokuStat.Model.Persistance
{
    public class FileManagerException : IOException
    {
        public FileManagerException() : base() { }
        public FileManagerException(string message) : base(message) { }
        public FileManagerException(string message, Exception innerException) : base(message, innerException) { }
    }
}
