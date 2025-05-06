using System.Text;

namespace VizsgaGyakWinForms.Persistence
{
    internal class FileManager : IFileManager
    {
        public void Save(string path, BlockTable table, int points)
        {
            List<string> data = new List<string>();
            StringBuilder sb = new StringBuilder();

            try
            {
                for (int i = 0; i < 4; i++)
                {
                    for (int j = 0; j < 4; j++)
                    {
                        sb.Append(table.GetData(i, j) ? "T" : "F");
                        sb.Append(",");
                    }
                    data.Add(sb.ToString());
                    sb.Clear();
                }
                data.Add(points.ToString());

                File.WriteAllLines(path, data);
            }
            catch (Exception e)
            {
                throw e;
            }
        }
        public (BlockTable, int) Load(string path)
        {
            string[] data;
            bool[,] table = new bool[4, 4];
            int points = -1;
            try
            {
                data = File.ReadAllText(path).Split(Environment.NewLine);

                for (int i = 0; i < 4; i++)
                {
                    string[] helper = data[i].Split(",");
                    for (int j = 0; j < 4; j++)
                    {
                        table[i, j] = helper[j] == "T";
                    }
                }
                points = int.Parse(data[4]);
                if (points < 0) throw new ArgumentOutOfRangeException("Cannot have negative points");
            }
            catch (Exception e)
            {
                throw e;
            }

            return (new BlockTable(table), points);
        }
    }
}
