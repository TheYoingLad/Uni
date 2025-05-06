
namespace Snake.Model
{
    public class TimerService : ITimerService, IDisposable
    {
        public event TimerCallback? Tick;

        private System.Threading.Timer? timer;

        public TimerService(TimerCallback timerCallback)
        {
            timer = null;
            Tick = timerCallback;
        }

        public void Start()
        {
            timer = new System.Threading.Timer(
                callback: new TimerCallback(Tick!),
                state: null,
                dueTime: 1000,
                period: 333);
        }

        public void Stop()
        {
            timer?.Dispose();
            timer = null;
        }

        public void Dispose()
        {
            timer?.Dispose();
        }
    }
}
