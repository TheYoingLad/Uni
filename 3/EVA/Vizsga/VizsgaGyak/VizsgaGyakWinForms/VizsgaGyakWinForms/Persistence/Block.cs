namespace VizsgaGyakWinForms.Persistence
{
    public class Block
    {
        public static List<(int, int)> GetBlockCoords(int id)
        {
            List<(int, int)> coords = new List<(int, int)>();
            switch (id)
            {
                case 0:
                    coords.Add((0, 0));
                    coords.Add((1, 0));
                    break;
                case 1:
                    coords.Add((0, 0));
                    coords.Add((0, 1));
                    break;
                case 2:
                    coords.Add((0, 0));
                    coords.Add((1, 0));
                    coords.Add((1, 1));
                    break;
                case 3:
                    coords.Add((0, 0));
                    coords.Add((0, 1));
                    coords.Add((1, 1));
                    break;
                default: throw new ArgumentOutOfRangeException("Invalid block id");
            }
            return coords;
        }
    }
}
