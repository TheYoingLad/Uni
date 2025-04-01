namespace LaserPigs.Persistence.Instructions
{
    public class Step : Instruction
    {
        private readonly Direction _direction;

        public Step(Direction direction) : base(0)
        {
            if ((int)direction > 3 || (int)direction < 0) throw new ArgumentOutOfRangeException(nameof(direction), "Invalid direction");
            _direction = direction;
        }

        public Direction GetDirection { get { return _direction; } }

        public Coordinate SimulateStep()
        {
            switch (_direction)
            {
                case Direction.Up:
                    return new Coordinate(-1, 0);
                case Direction.Right:
                    return new Coordinate(0, 1);
                case Direction.Down:
                    return new Coordinate(1, 0);
                case Direction.Left:
                    return new Coordinate(0, -1);
                default: return new Coordinate(0, 0);
            }
        }
        public override Instruction Clone()
        {
            return new Step(_direction);
        }

        public override string ToString()
        {
            return "Step " + _direction.ToString();
        }
    }
}
