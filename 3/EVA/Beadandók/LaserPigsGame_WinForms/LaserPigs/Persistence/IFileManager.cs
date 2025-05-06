namespace LaserPigs.Persistence
{
    public interface IFileManager
    {
        public (Player p1, Player p2, int instructionIndex) Load(string path);
        public void Save(string path, Player p1, Player p2, int instructionIndex);
    }
}
