namespace HfLabirintus
{
    enum Content { EMPTY, WALL, TREASURE, GHOST }
    struct Direction
    {
        public int x;
        public int y;
    }
    class Labirynth
    {
        private Content[,] map;

        public Labirynth(Content[,] content)
        {
            map = content;
        }
        public Content WhatIsThere(int x, int y, Direction dir)
        {
            if (!(0 <= x + dir.x && x + dir.x < map.GetLength(0) && 0 <= y + dir.y && y + dir.y < map.GetLength(1) && (dir.x == 0 || dir.y == 0))) throw new ArgumentException();
            return map[x + dir.x, y + dir.y];
        }
        public void Collect(int x, int y)
        {
            if (map[x, y] != Content.TREASURE) throw new ArgumentException();
            map[x, y] = Content.EMPTY;
        }
    }
    public class Program
    {
        static void Main(string[] args)
        {
            int n, m;
            string[] separatedLine = Console.ReadLine().Split();
            n = int.Parse(separatedLine[0]);
            m = int.Parse(separatedLine[1]);
            Content[,] map = new Content[n, m];
            Content placeholder;
            for (int i = 0; i < n; i++)
            {
                separatedLine = Console.ReadLine().Split();
                for (int j = 0; j < m; j++)
                {
                    switch (separatedLine[j])
                    {
                        case "Üres":
                            placeholder = Content.EMPTY;
                            map[i, j] = placeholder;
                            break;
                        case "Fal":
                            placeholder = Content.WALL;
                            map[i, j] = placeholder;
                            break;
                        case "Kincs":
                            placeholder = Content.TREASURE;
                            map[i, j] = placeholder;
                            break;
                        case "Szellem":
                            placeholder = Content.GHOST;
                            map[i, j] = placeholder;
                            break;
                    }
                }
            }
            Labirynth labirynth = new Labirynth(map);
            int x, y;
            try
            {
                separatedLine = Console.ReadLine().Split();
                x = int.Parse(separatedLine[0]);
                y = int.Parse(separatedLine[1]);
                labirynth.Collect(x, y);
                Console.WriteLine("Sikerült begyűjteni");
            }
            catch (Exception e)
            {
                Console.WriteLine("Nem sikerült a begyűjtés");
            }
            try
            {
                separatedLine = Console.ReadLine().Split();
                x = int.Parse(separatedLine[0]);
                y = int.Parse(separatedLine[1]);
                Direction dir;
                separatedLine = Console.ReadLine().Split();
                dir.x = int.Parse(separatedLine[0]);
                dir.y = int.Parse(separatedLine[1]);
                Content result = labirynth.WhatIsThere(x, y, dir);
                if (result == Content.TREASURE)
                    Console.WriteLine("Kincs");
                else if (result == Content.WALL)
                    Console.WriteLine("Fal");
                else if (result == Content.EMPTY)
                    Console.WriteLine("Üres");
                else
                    Console.WriteLine("Szellem");
            }
            catch (Exception e)
            {
                Console.WriteLine("Nem sikerült megtekinteni a tartalmat");
            }
        }
    }
}
