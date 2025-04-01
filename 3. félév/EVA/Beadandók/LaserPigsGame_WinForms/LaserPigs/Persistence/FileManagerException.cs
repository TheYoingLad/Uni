namespace LaserPigs.Persistence
{
    public class FileManagerException : Exception
    {
        //public FileManagerException() : base() { }
        //public FileManagerException(string message) : base(message) { }
        public FileManagerException(string message, Exception innerException) : base(message, innerException) { }
    }
}
