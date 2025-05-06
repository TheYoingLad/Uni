using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Minefield.Persistence
{
    /// <summary>
    /// Játékban résztvevő entitás absztakt típusa
    /// </summary>
    public abstract class Entity { }

    /// <summary>
    /// Játékos karakter típusa
    /// </summary>
    public class Player : Entity
    {
        private int _x; // sor
        private int _y; // oszlop

        /// <summary>
        /// Játékos karakter X-koordinátája (sor)
        /// </summary>
        public int X { get { return _x; } set { _x = value; } }

        /// <summary>
        /// Játékos karakter Y-koordinátája (oszlop)
        /// </summary>
        public int Y { get { return _y; } set { _y = value; } }

        /// <summary>
        /// Játékos karaktert példányosító konstruktor
        /// </summary>
        /// <param name="x">X-koordináta (sor)</param>
        /// <param name="y">Y-koordináta (oszlop)</param>
        public Player(int x, int y)
        {
            _x = x;
            _y = y;
        }

        /// <summary>
        /// Játékos karakter belső állapotának string-reprezentációja
        /// </summary>
        /// <returns>"player:{x-koordináta},{y-koordináta}"</returns>
        public override string ToString()
        {
            return $"player:{_x},{_y}";
        }
    }

    /// <summary>
    /// Akna absztrakt típusa
    /// </summary>
    public abstract class Mine : Entity
    {
        protected readonly int _timeToSink;     // Tick-ek száma / 1 cella süllyedés
        protected int _timeElapsedHere;           // Jelen cellában eltöltött tick-ek száma

        /// <summary>
        /// Akna süllyesztése
        /// </summary>
        /// <returns>Igaz ha süllyedt egy cellát</returns>
        public bool Descend()
        {
            _timeElapsedHere += 1;
            if (_timeElapsedHere >= _timeToSink)
            {
                _timeElapsedHere = 0;
                return true;
            }
            return false;
        }
        /// <summary>
        /// Új aknát példányosító konstruktor
        /// </summary>
        /// <param name="timeToSink"></param>
        protected Mine(int timeToSink)
        {
            _timeToSink = timeToSink;
            _timeElapsedHere = -20;
        }

        /// <summary>
        /// Adott belső állapotú aknát példányosító konsturktor
        /// </summary>
        /// <param name="timeToSink">Tick-ek száma / 1 cella süllyedés</param>
        /// <param name="timeElapsedHere">Jelen cellában eltöltött tick-ek száma</param>
        protected Mine(int timeToSink, int timeElapsedHere)
        {
            _timeToSink = timeToSink;
            _timeElapsedHere = timeElapsedHere;
        }

        /// <summary>
        /// Akna belső állapotának string-reprezentációja
        /// </summary>
        /// <returns>"{x-koordináta},{y-koordináta}"</returns>
        public override string ToString()
        {
            return $"{_timeToSink},{_timeElapsedHere}";
        }
    }


    /// <summary>
    /// Könnyű akna típusa
    /// </summary>
    public class EasyMine : Mine
    {
        /// <summary>
        /// Új könnyű aknát példányosító konstruktor
        /// </summary>
        public EasyMine() : base(15) { }

        /// <summary>
        /// Adott belső állapotú könnyű aknát példányosító konsturktor
        /// </summary>
        /// <param name="timeToSink">Tick-ek száma / 1 cella süllyedés</param>
        /// <param name="timeElapsedHere">Jelen cellában eltöltött tick-ek száma</param>
        public EasyMine(int timeToSink, int timeElapsedHere) : base(timeToSink, timeElapsedHere) { }

        /// <summary>
        /// Könnyű akna belső állapotának string-reprezentációja
        /// </summary>
        /// <returns>"easymine:{x-koordináta},{y-koordináta}"</returns>
        public override string ToString()
        {
            return $"easymine:" + base.ToString();
        }
    }

    /// <summary>
    /// Közepes akna típusa
    /// </summary>
    public class MediumMine : Mine
    {
        /// <summary>
        /// Új közepes aknát példányosító konstruktor
        /// </summary>
        public MediumMine() : base(8) { }

        /// <summary>
        /// Adott belső állapotú közepes aknát példányosító konsturktor
        /// </summary>
        /// <param name="timeToSink">Tick-ek száma / 1 cella süllyedés</param>
        /// <param name="timeElapsedHere">Jelen cellában eltöltött tick-ek száma</param>
        public MediumMine(int timeToSink, int timeElapsedHere) : base(timeToSink, timeElapsedHere) { }

        /// <summary>
        /// Közepes akna belső állapotának string-reprezentációja
        /// </summary>
        /// <returns>"mediummine:{x-koordináta},{y-koordináta}"</returns>
        public override string ToString()
        {
            return $"mediummine:" + base.ToString();
        }
    }

    /// <summary>
    /// Nehéz akna típusa
    /// </summary>
    public class HardMine : Mine
    {
        /// <summary>
        /// Új nehéz aknát példányosító konstruktor
        /// </summary>
        public HardMine() : base(4) { }

        /// <summary>
        /// Adott belső állapotú nehéz aknát példányosító konsturktor
        /// </summary>
        /// <param name="timeToSink">Tick-ek száma / 1 cella süllyedés</param>
        /// <param name="timeElapsedHere">Jelen cellában eltöltött tick-ek száma</param>
        public HardMine(int timeToSink, int timeElapsedHere) : base(timeToSink, timeElapsedHere) { }

        /// <summary>
        /// Nehéz akna belső állapotának string-reprezentációja
        /// </summary>
        /// <returns>"hardmine:{x-koordináta},{y-koordináta}"</returns>
        public override string ToString()
        {
            return $"hardmine:" + base.ToString();
        }
    }
}
