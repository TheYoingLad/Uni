namespace LaserPigs.Persistence.Instructions
{
    public class Shoot : Attack
    {
        public Shoot() : base(3) { }

        public override bool WouldBeHit(Coordinate attackerCoord, Coordinate targetCoord, Direction dir)
        {
            int attackerCoordX = attackerCoord.GetX;
            int attackerCoordY = attackerCoord.GetY;
            int targetCoordX = targetCoord.GetX;
            int targetCoordY = targetCoord.GetY;

            switch (dir)
            {
                case Direction.Up:
                    if (targetCoordY == attackerCoordY && targetCoordX < attackerCoordX) return true;
                    return false;
                case Direction.Right:
                    if (targetCoordX == attackerCoordX && targetCoordY > attackerCoordY) return true;
                    return false;
                case Direction.Down:
                    if (targetCoordY == attackerCoordY && targetCoordX > attackerCoordX) return true;
                    return false;
                case Direction.Left:
                    if (targetCoordX == attackerCoordX && targetCoordY < attackerCoordY) return true;
                    return false;
                default:
                    return false;
            }
        }

        public override string ToString()
        {
            return "Shoot";
        }
    }
}
