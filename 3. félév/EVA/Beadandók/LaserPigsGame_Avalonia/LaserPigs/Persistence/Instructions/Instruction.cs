namespace LaserPigs.Persistence.Instructions
{
    public abstract class Instruction
    {
        private readonly int _priority;

        public Instruction(int priority)
        {
            _priority = priority;
        }

        public int GetPriority { get { return _priority; } }

        public abstract Instruction Clone();
    }
}
