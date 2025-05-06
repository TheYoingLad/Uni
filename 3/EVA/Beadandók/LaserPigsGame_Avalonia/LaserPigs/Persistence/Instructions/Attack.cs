namespace LaserPigs.Persistence.Instructions
{
    public abstract class Attack : Instruction
    {
        public Attack(int priority) : base(priority) { }

        public abstract bool WouldBeHit(Coordinate attackerCoordinate, Coordinate targetCoordinate, Direction direction);
    }
}
