namespace LaserPigs.Model
{
    public class DamageEventArgs : EventArgs
    {
        private bool _isPlayer1;
        private int _newHp;

        public DamageEventArgs(bool isPlayer1, int newHp)
        {
            _isPlayer1 = isPlayer1;
            _newHp = newHp;
        }

        public bool GetIsPlayer1 { get { return _isPlayer1; } }
        public int GetNewHp { get { return _newHp; } }
    }
}
