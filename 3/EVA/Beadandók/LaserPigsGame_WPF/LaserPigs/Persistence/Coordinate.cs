namespace LaserPigs.Persistence
{
    public class Coordinate
    {
        private int _x;
        private int _y;

        public Coordinate(int x, int y)
        {
            _x = x;
            _y = y;
        }

        public int GetX { get { return _x; } }
        public int GetY { get { return _y; } }

        public static Coordinate operator +(Coordinate c1, Coordinate c2)
        {
            return new Coordinate(c1.GetX + c2.GetX, c1.GetY + c2.GetY);
        }
        public static bool operator ==(Coordinate c1, Coordinate c2)
        {
            return c1.GetX == c2.GetX && c1.GetY == c2.GetY;
        }
        public static bool operator !=(Coordinate c1, Coordinate c2)
        {
            return c1.GetX != c2.GetX || c1.GetY != c2.GetY;
        }
        public override string ToString()
        {
            return _x.ToString() + " " + _y.ToString();
        }

        public void InBounds(int size)
        {
            _x = Math.Max(0, _x);
            _x = Math.Min(_x, size - 1);

            _y = Math.Max(0, _y);
            _y = Math.Min(_y, size - 1);
        }
        public void Validate(int size)
        {
            if (_x < 0) throw new ArgumentOutOfRangeException(nameof(_x), "The X coordinate is less than 0");
            if (_x >= size) throw new ArgumentOutOfRangeException(nameof(_x), "The X coordinate is equal to or greater than the size of the map");
            
            if (_y < 0) throw new ArgumentOutOfRangeException(nameof(_y), "The Y coordinate is less than 0");
            if (_y >= size) throw new ArgumentOutOfRangeException(nameof(_y), "The Y coordinate is equal to or greater than the size of the map");
        }

        public override bool Equals(object? obj)
        {
            if (obj == null) throw new ArgumentNullException();
            if(ReferenceEquals (this, obj)) return true;

            if(obj is Coordinate c)
            {
                return c == this;
            }   
            return false;
        }
        public override int GetHashCode()
        {
            throw new NotImplementedException();
        }
    }
}
