using LaserPigs.Persistence.Instructions;

namespace LaserPigs.Model
{
    public class InstructionEventArgs
    {
        private Instruction _ins;
        private Instruction? _ins2;

        public InstructionEventArgs(Instruction ins)
        {
            _ins = ins.Clone();
        }
        public InstructionEventArgs(Instruction ins, Instruction ins2)
        {
            _ins = ins.Clone();
            _ins2 = ins2.Clone();
        }

        public Instruction GetInstruction { get { return _ins.Clone(); } }
        public Instruction? GetInstruction2 { get { return _ins2 != null ? _ins2.Clone() : null; } }
    }
}
