using System;

namespace VizsgaGyakAvalonia.Model
{
    public class StepArgs : EventArgs
    {
        private readonly int _x;
        private readonly int _y;
        private readonly bool? _wasKilled;
        private readonly int _info;

        public StepArgs(int x, int y, bool? wasKilled, int info)
        {
            _x = x;
            _y = y;
            _wasKilled = wasKilled;
            _info = info;
        }

        public int X => _x;
        public int Y => _y;
        public bool? wasKilled => _wasKilled;
        public int Info => _info;
    }
}
