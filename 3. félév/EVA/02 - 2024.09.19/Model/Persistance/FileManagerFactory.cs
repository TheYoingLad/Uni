namespace DokuStat.Model.Persistance
{
    public class FileManagerFactory
    {
        public static IFileManager? CreateForPath(string path)
        {
            switch (Path.GetExtension(path))
            {
                case ".txt":
                    return new TextFileManager(path);
                case ".pdf":
                    return new PdfFileManager(path);
                default:
                    return null;
            }
        }
    }
}
