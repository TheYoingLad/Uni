namespace Minefield.Model
{
    public class MinefieldTimer : System.Timers.Timer, IMinefieldTimer
    {
        public MinefieldTimer(): base(100) { }
    }
}
