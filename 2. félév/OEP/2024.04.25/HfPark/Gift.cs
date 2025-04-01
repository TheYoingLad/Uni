using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HfPark
{
    public class Gift
    {
        private Size size;
        public TargetShot? targetShot;

        public Gift(Size s)
        {
            size = s;
        }

        public int Value() { return Point() * size.Multi(); }
        public virtual int Point() { return 0; }
    }

    public class Ball : Gift
    {
        public Ball(Size s) : base(s) { }
        public override int Point() { return 1; }
    }

    public class Figure : Gift
    {
        public Figure(Size s) : base(s) { }
        public override int Point() { return 2; }
    }

    public class Plush : Gift
    {
        public Plush(Size s) : base(s) { }
        public override int Point() { return 3; }
    }
}
