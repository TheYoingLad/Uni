using LaserPigs.Persistence.Instructions;

namespace LaserPigs.Model
{
    public class InstructionEventArgs
    {
        private Instruction _ins;
        private Instruction? _ins2;

        public InstructionEventArgs(Instruction ins)
        {
            _ins = ins;
        }
        public InstructionEventArgs(Instruction ins, Instruction ins2)
        {
            _ins = ins;
            _ins2 = ins2;
        }

        public Instruction GetInstruction { get { return _ins; } }
        public Instruction? GetInstruction2 { get { return _ins2; } }
    }
}
