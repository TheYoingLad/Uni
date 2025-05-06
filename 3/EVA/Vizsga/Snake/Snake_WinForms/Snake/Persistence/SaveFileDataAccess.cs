using Snake.Model;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net.Http.Headers;
using System.Text;
using System.Threading.Tasks;

namespace Snake.Persistence
{
    public class SaveFileDataAccess : IDataAccess
    {
        private string path;

        public SaveFileDataAccess(string path)
        {
            this.path = path;

            if (!File.Exists(path)) throw new FileNotFoundException("The save file was not found!");
            if (Path.GetExtension(path) != ".save") throw new ExtensionException("Extension of file was not .save!");
        }

        public (int, List<Coordinate>) Load()
        {
            using (StreamReader sr = new StreamReader(path))
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
    }
}
