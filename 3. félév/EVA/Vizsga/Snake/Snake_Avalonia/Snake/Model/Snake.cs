namespace Snake.Model
{
    public class Snake
    {
        #region Fields and Properties

        public Direction Direction { get; private set; }
        public Queue<Coordinate> SnakeBody {  get; private set; }
        public Coordinate Head { get {  return SnakeBody.Last().Copy(); } }

        private readonly int mapResolution;

        #endregion

        #region Constructor(s)

        /// <exception cref="ArgumentException">When map is too small for the given length</exception>
        public Snake(int mapResolution, int length = 5)
        {
            // Check if Snake fits
            if (mapResolution < length + 1) throw new ArgumentException($"The map resolution ({mapResolution}) is too small to create a snake!");
            this.mapResolution = mapResolution;

            // Initiate variables
            Direction = Direction.UP;
            SnakeBody = new Queue<Coordinate>();

            Coordinate headPos = new Coordinate(mapResolution / 2, mapResolution / 2 + length / 2);
            SnakeBody.Enqueue(headPos);

            // Build SnakeBody
            for (int i = 1; i < length; i++)
            {
                Coordinate bodyPos = new Coordinate(headPos.X, headPos.Y - i);
                SnakeBody.Enqueue(bodyPos);
            }
        }

        #endregion

        #region Public Functions

        /// <summary>
        /// Moves the snake body forward (to the direction it faces) and
        /// returns the positions were there were a change of tile
        /// </summary>
        /// <param name="eggs">List of all the egg's positions that can be eaten</param>
        /// <returns>Returns a tuple which first element is the new head position and
        /// the second is either null (if snake consumed an egg) or the position which was uncoverd by the tail</returns>
        public (Coordinate, Coordinate?) StepForward(List<Coordinate>? eggs = null)
        {
            Coordinate newHeadPos = Direction.ToVec();
            newHeadPos += SnakeBody.Last();

            // wrap around the field (like torus)
            if (newHeadPos.X < 0) newHeadPos.X = mapResolution - 1;
            else if (newHeadPos.X >= mapResolution) newHeadPos.X = 0;
            if (newHeadPos.Y < 0) newHeadPos.Y = mapResolution - 1;
            else if (newHeadPos.Y >= mapResolution) newHeadPos.Y = 0;

            // add new head element and remove last bodypart (if neccessary) of snake
            SnakeBody.Enqueue(newHeadPos);
            Coordinate? removed = (eggs?.Contains(newHeadPos) ?? false) ? null : SnakeBody.Dequeue();

            return (newHeadPos, removed);
        }

        /// <summary>
        /// Turns the snake left relative to its facing
        /// </summary>
        public void TurnLeft()
        {
            Direction = Direction.TurnLeft();
        }

        /// <summary>
        /// Turns the snake rigth relative to its facing
        /// </summary>
        public void TurnRight()
        {
            Direction = Direction.TurnRight();
        }

        /// <summary>
        /// Copies the object and gives back an exact replica
        /// </summary>
        public Snake Copy()
        {
            Snake copy = new Snake(this.mapResolution);
            copy.Direction = this.Direction;
            copy.SnakeBody = new Queue<Coordinate>(this.SnakeBody);

            return copy;
        }

        #endregion
    }
}