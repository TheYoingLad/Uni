namespace LaserPigs.Persistence.Instructions
{
    public class Punch : Attack
    {
        public Punch() : base(2) { }

        public override bool WouldBeHit(Coordinate attackerCoord, Coordinate targetCoord, Direction direction)
        {
            int attackerCoordX = attackerCoord.GetX;
            int attackerCoordY = attackerCoord.GetY;
            int targetCoordX = targetCoord.GetX;
            int targetCoordY = targetCoord.GetY;

            for (int i = -1; i <= 1; i++) for (int j = -1; j <= 1; j++) if (attackerCoordX + i == targetCoordX && attackerCoordY + j == targetCoordY) return true;
            return false;
        }

        public override string ToString()
        {
            return "Punch";
        }
    }
}
