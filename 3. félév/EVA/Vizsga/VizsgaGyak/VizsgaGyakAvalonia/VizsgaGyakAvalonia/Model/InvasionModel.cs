using System;
using System.Timers;
using VizsgaGyakAvalonia.Persistence;

namespace VizsgaGyakAvalonia.Model
{
    public class InvasionModel
    {
        private Timer _timer;
        private Soldier?[,] _battlefield;
        private TimeSpan _time;
        private int _hp;
        private int _placeable;
        private int _killsUntillNextPlaceable;
        private bool _isPaused;
        private Random _random;

        public InvasionModel()
        {
            _timer = new Timer(100);
            _timer.Elapsed += TimerTick;

            _battlefield = new Soldier?[10, 20];
            _battlefield[1, 0] = new Enemy();
            _battlefield[3, 19] = new Friendly();

            _time = new TimeSpan(0, 0, 0);
            _hp = 3;
            _placeable = 2;
            _killsUntillNextPlaceable = 3;
            _isPaused = true;
            _random = new Random();
        }

        public TimeSpan GameTime => _time;
        public bool IsPaused => _isPaused;
        public int Placeable => _placeable;
        public int HP => _hp;
        public string this[int x, int y] => _battlefield[x, y]?.ToString() ?? "";


        public event EventHandler<StepArgs>? Step;
        public event EventHandler? AdvanceTime;
        public event EventHandler? GameOver;



        public void TimerResume()
        {
            _isPaused = false;
            _timer.Start();
        }
        public void TimerPause()
        {
            _isPaused = true;
            _timer.Stop();
        }


        private void TimerTick(object? sender, EventArgs e)
        {
            if (_isPaused) return;

            _time = _time.Add(new TimeSpan(0, 0, 0, 0, 100));
            for (int i = 0; i < 10; i++)
            {
                for (int j = 0; j < 20; j++)
                {
                    if (_battlefield[i, j]?.Step() ?? false)
                    {
                        Soldier? s = _battlefield[i, j];
                        _battlefield[i, j] = null;

                        if (j == 19)
                        {
                            _hp--;
                            OnStep(i, j, false);
                            if (_hp == 0) OnGameOver();
                        }
                        else
                        {
                            _battlefield[i, j + 1] = s;

                            if (NextToAlly(i, j + 1))
                            {
                                UpdatePlaceable();
                                OnStep(i, j, true);
                            }
                            else OnStep(i, j, null);
                        }
                    }
                }
            }

            OnAndvanceTime();
        }

        private bool NextToAlly(int x, int y)
        {
            if (x == 0) return _battlefield[1, y] is Friendly;
            if (x == 9) return _battlefield[8, y] is Friendly;
            return _battlefield[x - 1, y] is Friendly || _battlefield[x + 1, y] is Friendly;
        }
        private void UpdatePlaceable()
        {
            if (--_killsUntillNextPlaceable == 0)
            {
                _placeable++;
                _killsUntillNextPlaceable = 3;
            }
        }

        private void OnStep(int x, int y, bool? wasKilled)
        {
            if(wasKilled == null) Step?.Invoke(this, new StepArgs(x, y, null, 0));
            else if ((bool)wasKilled) Step?.Invoke(this, new StepArgs(x, y, true, _placeable));
            else Step?.Invoke(this, new StepArgs(x, y, false, _hp));
        }
        private void OnGameOver()
        {
            TimerPause();
            GameOver?.Invoke(this, EventArgs.Empty);
        }
        private void OnAndvanceTime()
        {
            AdvanceTime?.Invoke(this, EventArgs.Empty);
        }
    }
}
