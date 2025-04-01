namespace LaserPigs.Persistence.Instructions
{
    public class Turn : Instruction
    {
        private readonly Rotation _rotation;

        public Turn(Rotation rotation) : base(1)
        {
            if ((int)rotation != -1 && (int)rotation != 1) throw new ArgumentOutOfRangeException(nameof(rotation), "Invalid rotation");
            _rotation = rotation;
        }

        public Rotation GetRotation { get { return _rotation; } }

        public override Instruction Clone()
        {
            return new Turn(_rotation);
        }

        public override string ToString()
        {
            return "Turn " + _rotation.ToString();
        }
    }
}
