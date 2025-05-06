using Minefield.Model;

namespace Minefield.Persistence
{
    /// <summary>
    /// Aknamező játék fájlkezelő típusa
    /// </summary>
    public class MinefieldFileAccess : IMinefieldFileAccess
    {
        /// <summary>
        /// Fájl betöltése
        /// </summary>
        /// <param name="path">Fájl elérési útvonala</param>
        /// <returns>(Játékteret reprezentáló Entitiy mátrix, Játékos objektum, Játékidő) rendezett hármas</returns>
        public (Entity?[,], Player, TimeSpan) LoadFile(string path)
        {
            try
            {
                using (StreamReader sr = new StreamReader(path))
                {
                    int milliseconds = int.Parse(sr.ReadLine() ?? String.Empty);
                    TimeSpan gameTime = new TimeSpan(0,0,0,0,milliseconds);
                    int rows = int.Parse(sr.ReadLine() ?? String.Empty);
                    int cols = int.Parse(sr.ReadLine() ?? String.Empty);
                    Entity?[,] entities = new Entity?[rows, cols];
                    Player? player = null;

                    string line;
                    string[] lineSplit;
                    string[] entityData;
                    for (int i = 0; i < rows; i++)
                    {
                        for (int j = 0; j < cols; j++)
                        {
                            line = sr.ReadLine() ?? String.Empty;
                            if(line == "0")
                            {
                                entities[i,j] = null;
                            }
                            else
                            {
                                lineSplit = line.Split(':');
                                entityData = lineSplit[1].Split(',');
                                switch (lineSplit[0])
                                {
                                    case "player":
                                        entities[i, j] = new Player(int.Parse(entityData[0]), int.Parse(entityData[1]));
                                        player = (Player)entities[i, j]!;
                                        break;
                                    case "easymine":
                                        entities[i, j] = new EasyMine(int.Parse(entityData[0]), int.Parse(entityData[1]));
                                        break;
                                    case "mediummine":
                                        entities[i, j] = new MediumMine(int.Parse(entityData[0]), int.Parse(entityData[1]));
                                        break;
                                    case "hardmine":
                                        entities[i, j] = new HardMine(int.Parse(entityData[0]), int.Parse(entityData[1]));
                                        break;
                                }
                            }
                        }
                    }
                    if (player is null) throw new Exception();

                    return (entities, player, gameTime);
                }
            }
            catch
            {
                throw new MinefieldFileAccessException();
            }
        }

        /// <summary>
        /// Fájl mentése
        /// </summary>
        /// <param name="path">Fájl elérési útvonala</param>
        /// <param name="time">Játékidő a mentéskor</param>
        /// <param name="e">Játékteret reprezentáló Entitiy mátrix</param>
        public void SaveFile(string path, TimeSpan time, Entity?[,] e)
        {
            try
            {
                using (StreamWriter sw = new StreamWriter(path))
                {
                    sw.WriteLine(time.TotalMilliseconds.ToString());
                    sw.WriteLine(e.GetLength(0));
                    sw.WriteLine(e.GetLength(1));
                    for (int i = 0; i < e.GetLength(0); i++)
                    {
                        for (int j = 0; j < e.GetLength(1); j++)
                        {
                            if (e[i,j] is not null)
                            {
                            sw.WriteLine(e[i, j]!.ToString());
                            }
                            else
                            {
                                sw.WriteLine("0");
                            }
                        }
                    }
                }
            }
            catch
            {
                throw new MinefieldFileAccessException();
            }
        }
    }
}
