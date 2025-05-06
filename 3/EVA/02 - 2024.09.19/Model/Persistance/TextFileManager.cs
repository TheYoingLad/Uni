namespace DokuStat.Model.Persistance
{
    public class TextFileManager : IFileManager
    {
        private readonly string _path;

        public TextFileManager(string path)
        {
            _path = path;
        }

        public async Task<string> LoadAsync()
        {
            try
            {
                return await File.ReadAllTextAsync(_path);
            }
            catch (Exception ex)
            {
                throw new FileManagerException(ex.Message, ex);
            }
        }
    }
}
