using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Snake.Model
{
    public enum TileType
    {
        Empty, Snake, SnakeHead, Egg, Wall
    }

    public class Tile
    {
        public TileType type {  get; set; }

        public Tile()
        {
            type = TileType.Empty;
        }

        public Tile(TileType type)
        {
            this.type = type;
        }

        /// <summary>
        /// Copies the current object
        /// </summary>
        /// <returns>Exact replica of the object</returns>
        public Tile Copy()
        {
            return new Tile(type);
        }
    }
}
