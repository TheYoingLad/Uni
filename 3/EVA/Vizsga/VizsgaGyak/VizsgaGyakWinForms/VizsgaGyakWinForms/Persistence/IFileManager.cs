namespace VizsgaGyakWinForms.Persistence
{
    public interface IFileManager
    {
        public void Save(string path, BlockTable table, int points);
        public (BlockTable, int) Load(string path);
    }
}
