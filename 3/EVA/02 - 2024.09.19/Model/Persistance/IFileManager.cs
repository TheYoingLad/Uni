namespace DokuStat.Model.Persistance
{
    public interface IFileManager
    {
        public Task<string> LoadAsync();
    }
}
