using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace kert
{
    public abstract class Növényfajta
    {
        private int érésiIdő;

        protected Növényfajta(int i) { érésiIdő = i; }

        public int getÉrésiIdő() { return érésiIdő; }
        public virtual bool isZöldség() { return false; }
        public virtual bool isVirág() { return false; }
    }
    public abstract class Virág : Növényfajta
    {
        protected Virág(int i) : base(i) { }

        public override bool isVirág() { return true; }
    }
    public abstract class Zöldség : Növényfajta
    {
        protected Zöldség(int i) : base(i) { }

        public override bool isZöldség() { return true; }
    }
    public class Burgonya : Zöldség
    {
        protected Burgonya() : base(4) { }
        private static Burgonya? instance;
        public Burgonya Instance()
        {
            if (instance == null) instance = new Burgonya();
            return instance;
        }
    }
    public class Kukorica : Zöldség
    {
        protected Kukorica() : base(2) { }
        private static Kukorica? instance;
        public Kukorica Instance()
        {
            if (instance == null) instance = new Kukorica();
            return instance;
        }
    }
    public class Tarhonya : Zöldség
    {
        protected Tarhonya() : base(10) { }
        private static Tarhonya? instance;
        public Tarhonya Instance()
        {
            if (instance == null) instance = new Tarhonya();
            return instance;
        }
    }
    public class Tulipán : Virág
    {
        protected Tulipán() : base(2) { }
        private static Tulipán? instance;
        public Tulipán Instance()
        {
            if (instance == null) instance = new Tulipán();
            return instance;
        }
    }
    public class Játszint : Virág
    {
        protected Játszint() : base(3) { }
        private static Játszint? instance;
        public Játszint Instance()
        {
            if (instance == null) instance = new Játszint();
            return instance;
        }
    }
    public class Árvácska : Virág
    {
        protected Árvácska() : base(1) { }
        private static Árvácska? instance;
        public Árvácska Instance()
        {
            if (instance == null) instance = new Árvácska();
            return instance;
        }
    }
}
