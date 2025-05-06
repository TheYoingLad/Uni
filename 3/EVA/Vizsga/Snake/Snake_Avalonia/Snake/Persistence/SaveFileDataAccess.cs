using Snake.Model;

namespace Snake.Persistence
{
    public class SaveFileDataAccess : IDataAccess
    {
        private string? path;
        private Stream stream;

        public SaveFileDataAccess(string path)
        {
            if (!File.Exists(path)) throw new FileNotFoundException("The save file was not found!");
            if (Path.GetExtension(path) != ".save") throw new ExtensionException("Extension of file was not .save!");

            this.path = path;
            stream = File.OpenRead(path);
        }

        public SaveFileDataAccess(Stream stream)
        {
            if (stream == null) throw new ArgumentNullException("Stream is null and file cannot be read!");

            this.stream = stream;
        }

        public (int, List<Coordinate>) Load()
        {
            if (path == null) throw new ArgumentNullException("Path is null and file cannot be read!");

            using (StreamReader sr = new StreamReader(stream))
            {
                int resolution;
                if (!int.TryParse(sr.ReadLine()!, out resolution))
                    throw new FormatException("Dimensions of field is not a number!");

                List<Coordinate> walls = new List<Coordinate>();

                Coordinate pos = new Coordinate(0, 0);
                while (!sr.EndOfStream)
                {
                    string? line = sr.ReadLine();
                    if (line == null) throw new FormatException("Null valued line was read in save file!");

                    foreach (char c in line)
                    {
                        if (c == '#') walls.Add(pos.Copy());

                        pos.X += 1;
                        if (pos.X >= resolution)
                        {
                            break;
                        }
                    }

                    pos.X  = 0;
                    pos.Y += 1;
                    if (pos.Y >= resolution) break;
                }

                return (resolution, walls);
            }
        }

        public async Task<(int, List<Coordinate>)> LoadAsync()
        {
            if (path == null && stream == null) throw new ArgumentNullException("Path and Stream are null and file cannot be read!");
            stream ??= File.OpenRead(path!);

            using (StreamReader sr = new StreamReader(stream))
            {
                int resolution;
                string? first_line = await sr.ReadLineAsync();
                if (!int.TryParse(first_line ?? "", out resolution))
                    throw new FormatException("Dimensions of field is not a number!");


                List<Coordinate> walls = new List<Coordinate>();

                Coordinate pos = new Coordinate(0, 0);
                while (!sr.EndOfStream)
                {
                    string? line = await sr.ReadLineAsync();
                    if (line == null) throw new FormatException("Null valued line was read in save file!");

                    foreach (char c in line)
                    {
                        if (c == '#') walls.Add(pos.Copy());

                        pos.X += 1;
                        if (pos.X >= resolution)
                        {
                            break;
                        }
                    }

                    pos.X = 0;
                    pos.Y += 1;
                    if (pos.Y >= resolution) break;
                }

                return (resolution, walls);
            }
        }
    }
}
