namespace Snake.Model
{
    public interface ITimerService
    {
        event TimerCallback Tick;
        void Start();
        void Stop();
    }
}
