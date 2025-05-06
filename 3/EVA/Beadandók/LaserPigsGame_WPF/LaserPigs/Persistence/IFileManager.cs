namespace LaserPigs.Persistence
{
    public interface IFileManager
    {
        public Task<(Player p1, Player p2, int instructionIndex)> LoadAsync(string path);
        public Task SaveAsync(string path, Player p1, Player p2, int instructionIndex);
    }
}
