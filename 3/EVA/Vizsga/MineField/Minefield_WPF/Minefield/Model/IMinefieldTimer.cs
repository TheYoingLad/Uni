using System.Timers;

namespace Minefield.Model
{
    public interface IMinefieldTimer : IDisposable
    {
        void Start();
        void Stop();
        event ElapsedEventHandler Elapsed;
    }
}
