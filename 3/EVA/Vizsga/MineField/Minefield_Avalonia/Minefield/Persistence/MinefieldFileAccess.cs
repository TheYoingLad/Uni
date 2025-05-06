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
        public async Task<(Entity?[,], Player, TimeSpan)> LoadFileAsync(string path) => await LoadFileAsync(File.OpenRead(path));

        /// <summary>
        /// Fájl betöltése
        /// </summary>
        /// <param name="stream">Adatfolyam</param>
        /// <returns>(Játékteret reprezentáló Entitiy mátrix, Játékos objektum, Játékidő) rendezett hármas</returns>
        public async Task<(Entity?[,], Player, TimeSpan)> LoadFileAsync(Stream stream)
        {
            try
            {
                using (StreamReader sr = new StreamReader(stream))
                {
                    int milliseconds = int.Parse(await sr.ReadLineAsync() ?? String.Empty);
                    TimeSpan gameTime = new TimeSpan(0,0,0,0,milliseconds);
                    int rows = int.Parse(await sr.ReadLineAsync() ?? String.Empty);
                    int cols = int.Parse(await sr.ReadLineAsync() ?? String.Empty);
                    Entity?[,] entities = new Entity?[rows, cols];
                    Player? player = null;

                    string line;
                    string[] lineSplit;
                    string[] entityData;
                    for (int i = 0; i < rows; i++)
                    {
                        for (int j = 0; j < cols; j++)
                        {
                            line = await sr.ReadLineAsync() ?? String.Empty;
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
        public async Task SaveFileAsync(string path, TimeSpan time, Entity?[,] e) => await SaveFileAsync(File.OpenWrite(path), time, e);

        /// <summary>
        /// Fájl mentése
        /// </summary>
        /// <param name="stream">Adatfolyam</param>
        /// <param name="time">Játékidő a mentéskor</param>
        /// <param name="e">Játékteret reprezentáló Entitiy mátrix</param>
        public async Task SaveFileAsync(Stream stream, TimeSpan time, Entity?[,] e)
        {
            try
            {
                using (StreamWriter sw = new StreamWriter(stream))
                {
                    await sw.WriteLineAsync(time.TotalMilliseconds.ToString());
                    await sw.WriteLineAsync(e.GetLength(0).ToString());
                    await sw.WriteLineAsync(e.GetLength(1).ToString());
                    for (int i = 0; i < e.GetLength(0); i++)
                    {
                        for (int j = 0; j < e.GetLength(1); j++)
                        {
                            if (e[i,j] is not null)
                            {
                            await sw.WriteLineAsync(e[i, j]!.ToString());
                            }
                            else
                            {
                                await sw.WriteLineAsync("0");
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
