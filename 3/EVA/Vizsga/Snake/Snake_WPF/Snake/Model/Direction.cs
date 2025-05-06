using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Snake.Model
{
    public enum Direction
    {
        RIGHT = 0, UP = 1, LEFT = 2, DOWN = 3
    }

    public static class Extension
    {
        private static Direction Turn(Direction dir, int to)
        {
            if (dir + to < 0) return Direction.DOWN;
            Direction direction = (dir + to);
            return (Direction)((int)direction % 4);
        }

        public static Direction TurnLeft(this Direction direction)
        {
            return Turn(direction, 1);
        }

        public static Direction TurnRight(this Direction direction)
        {
            return Turn(direction, -1);
        }

        public static Coordinate ToVec(this Direction direction)
        {
            switch (direction)
            {
                case Direction.UP:
                    return new Coordinate(0, -1);
                case Direction.DOWN:
                    return new Coordinate(0, 1);
                case Direction.LEFT:
                    return new Coordinate(-1, 0);
                case Direction.RIGHT:
                    return new Coordinate(1, 0);
                default:
                    return new Coordinate(0, 0);
            }
        }
    }
}
