using LaserPigs.Persistence.Instructions;

namespace LaserPigs.Persistence
{
    /// <summary>
    /// The enumeration for directions
    /// </summary>
    public enum Direction { Up, Right, Down, Left, None }
    /// <summary>
    /// The enumeration for rotations
    /// </summary>
    public enum Rotation { CW = 1, CCW = -1 }

    /// <summary>
    /// A player for the Laser Pigs game
    /// </summary>
    public class Player
    {
        #region Constants
        private const int BaseMaxHp = 3;
        private const int BaseHp = 3;
        private const int BaseInstructionsLength = 5;
        #endregion

        #region Fields
        private readonly int _size;
        private Coordinate _coordinate;
        private readonly int _maxHp;
        private int _hp;
        private Direction _direction;
        private Instruction[] _instructions;
        private int _instructionCount;
        private bool _instructionsLocked;
        #endregion

        #region Constructors
        /// <summary>
        /// Constructs a Player with base parametres
        /// </summary>
        /// <param name="isP1">Is this player Player 1</param>
        /// <param name="size">Size of the map the player is on</param>
        public Player(bool isP1, int size) : this(size,
                                                  StartingCoordinate(isP1, size),
                                                  BaseMaxHp,
                                                  BaseHp,
                                                  StartingDirection(isP1),
                                                  BaseInstructionsLength)
        { }
        /// <summary>
        ///  Constructs a Player with custom parametres
        /// </summary>
        /// <param name="size">Size of the map the player is on</param>
        /// <param name="coordinate">Coordinate of the player</param>
        /// <param name="maxHp">Max HP of the player</param>
        /// <param name="hp">Current HP of the player</param>
        /// <param name="direction">Direction the player is facing</param>
        /// <param name="instructionsLength">Length of the instructions array of the player</param>
        /// <exception cref="ArgumentOutOfRangeException"></exception>
        public Player(int size, Coordinate coordinate, int maxHp, int hp, Direction direction, int instructionsLength)
        {
            if (size < 2) throw new ArgumentOutOfRangeException(nameof(size), "The map size is less than 2");
            _size = size;

            coordinate.Validate(size);
            _coordinate = coordinate;

            if (maxHp < 1) throw new ArgumentOutOfRangeException(nameof(maxHp), "The maximum HP value is less than 1");
            _maxHp = maxHp;

            if (hp > maxHp) throw new ArgumentOutOfRangeException(nameof(hp), "The HP value is more than the maximum HP value");
            if (hp < 1) throw new ArgumentOutOfRangeException(nameof(hp), "The HP value is less than 1");
            _hp = hp;

            if (direction == Direction.None) throw new ArgumentOutOfRangeException(nameof(direction), "Invalid direction");
            _direction = direction;

            if (instructionsLength < 1) throw new ArgumentOutOfRangeException(nameof(instructionsLength), "The instructions array length is less than 1");
            _instructions = new Instruction[instructionsLength];

            _instructionCount = 0;

            _instructionsLocked = false;
        }
        #endregion

        #region Properties
        /// <summary>
        /// Gets the coordinate of the player
        /// </summary>
        public Coordinate GetCoordinate { get { return _coordinate; } }
        /// <summary>
        /// Gets the current HP of the player
        /// </summary>
        public int GetHp { get { return _hp; } }
        /// <summary>
        /// Gets the max HP of the player
        /// </summary>
        public int GetMaxHp { get { return _maxHp; } }
        /// <summary>
        /// Gets whether the player is dead or not
        /// </summary>
        public bool IsDead { get { return _hp == 0; } }
        /// <summary>
        /// Gets the direction the player is facing
        /// </summary>
        public Direction GetDirection { get { return _direction; } }
        /// <summary>
        /// Gets a copy of the instructions array of the player
        /// </summary>
        public Instruction[] GetInstructions
        {
            get
            {
                Instruction[] instructions = new Instruction[_instructions.Length];
                _instructions.CopyTo(instructions, 0);
                return instructions;
            }
        }
        /// <summary>
        /// Gets the size of the map the player is on
        /// </summary>
        public int GetSize { get { return _size; } }
        /// <summary>
        /// Gets the length of the instructions array
        /// </summary>
        public int GetMaxInstructions { get { return _instructions.Length; } }
        /// <summary>
        /// Gets the current amount of instructions in the instructions array
        /// </summary>
        public int GetInstructionCount { get { return _instructionCount; } }
        /// <summary>
        /// Gets whether the instructions array is empty or not
        /// </summary>
        public bool IsEmpty { get { return _instructionCount == 0; } }
        /// <summary>
        /// Gets whether the instructions array is full or not
        /// </summary>
        public bool IsFull { get { return _instructionCount == _instructions.Length; } }
        /// <summary>
        /// Gets or sets the instructions array to locked
        /// </summary>
        public bool Locked
        {
            get { return _instructionsLocked; }
            set
            {
                if (value && !IsFull) throw new InvalidOperationException("The array of instructions is not full");
                _instructionsLocked = value;
            }
        }
        #endregion

        #region Public methods
        /// <summary>
        /// Adds the delta coordinate to the player's coordinate and sets it in bounds
        /// </summary>
        /// <param name="deltaCoord">Delta coordinate</param>
        public void StepTo(Coordinate deltaCoord)
        {
            _coordinate += deltaCoord;
            _coordinate.InBounds(_size);
        }
        /// <summary>
        /// Changes the direction the player is facing, turning it in the direction of the parameter
        /// </summary>
        /// <param name="rot">Direction of rotation</param>
        /// <exception cref="ArgumentOutOfRangeException"></exception>
        public void Turn(Rotation rot)
        {
            int deltaDir = (int)rot;

            if (deltaDir != -1 && deltaDir != 1) throw new ArgumentOutOfRangeException(nameof(rot), "Invalid rotation");

            int newDir = ((int)_direction + deltaDir + 4) % 4;

            _direction = (Direction)newDir;
        }
        /// <summary>
        /// Lowers the HP of the player by 1, but not below 0
        /// </summary>
        public void TakeDamage()
        {
            TakeDamage(1);
        }
        /// <summary>
        /// Adds an instruction to the instructions array of the player if it is not full or locked
        /// </summary>
        /// <param name="ins">Instruction to be added</param>
        /// <exception cref="InvalidOperationException"></exception>
        public void AddInstruction(Instruction ins)
        {
            if (IsFull) throw new InvalidOperationException("The array of instructions is full");
            if (Locked) throw new InvalidOperationException("The array of instructions is locked");
            _instructions[_instructionCount] = ins;
            _instructionCount++;
        }
        /// <summary>
        /// Removes an instruction from the instructions array of the player if it is not empty or locked
        /// </summary>
        /// <exception cref="InvalidOperationException"></exception>
        public void RemoveInstruction()
        {
            if (IsEmpty) throw new InvalidOperationException("The array of instructions is empty");
            if (Locked) throw new InvalidOperationException("The array of instructions is locked");
            _instructionCount--;
        }
        /// <summary>
        /// Returns the instruction from the instructions array at the given index if the index is valid
        /// </summary>
        /// <param name="index">Index in question</param>
        /// <returns>Instruction at the given index</returns>
        /// <exception cref="IndexOutOfRangeException"></exception>
        public Instruction GetInstruction(int index)
        {
            if (index >= _instructionCount) throw new IndexOutOfRangeException("The index is out of range");
            return _instructions[index];
        }
        /// <summary>
        /// Resets the array of instructions by resetting the stored index
        /// </summary>
        public void ResetInstructions()
        {
            _instructionCount = 0;
            Locked = false;
        }
        #endregion

        #region Private methods
        private void TakeDamage(int damage)
        {
            if (damage < 1) throw new ArgumentOutOfRangeException(nameof(damage), "The damage value is less than 1");
            _hp = Math.Max(0, _hp - damage);
        }
        public static Coordinate StartingCoordinate(bool isP1, int size)
        {
            if (size < 2) throw new ArgumentOutOfRangeException(nameof(size), "The map size is less than 2");
            int x = (isP1 ? 0 : size - 1);
            int y = (isP1 ? (size / 2) - 1 : (int)Math.Ceiling((decimal)size / 2));
            return new Coordinate(x, y);
        }
        public static Direction StartingDirection(bool isP1)
        {
            return isP1 ? Direction.Down : Direction.Up;
        }
        #endregion
    }
}
