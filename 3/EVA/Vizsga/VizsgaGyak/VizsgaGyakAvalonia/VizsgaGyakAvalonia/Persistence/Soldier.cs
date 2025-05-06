namespace VizsgaGyakAvalonia.Persistence
{
    public abstract class Soldier
    {
        //protected Soldier() { }

        public abstract override string ToString();
        public abstract bool Step();
    }

    public class Friendly : Soldier
    {
        public Friendly() : base() { }

        public override string ToString()
        {
            return "<";
        }

        public override bool Step()
        {
            return false;
        }
    }

    public class Enemy : Soldier
    {
        private int _timeLeftToMove;
        public Enemy() : base()
        {
            _timeLeftToMove = 10;
        }

        public override string ToString()
        {
            return ">";
        }

        public override bool Step()
        {
            if(_timeLeftToMove == 0)
            {
                _timeLeftToMove = 10;
                return true;
            }
            _timeLeftToMove--;
            return false;
        }
    }
}
