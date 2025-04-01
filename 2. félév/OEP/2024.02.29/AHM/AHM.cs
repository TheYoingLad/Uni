using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AHM
{
    class AHM
    {
        private double[] x;
        private int dim;

        public AHM(int m)
        {
            x = new double[(m*(m+1))/2];
            dim = m;
        }

        public int Ind(int i, int j)
        {
            return j + i * (i + 1) / 2;
        }

        public double Get(int i, int j)
        {
            if (!(0 <= i && i < dim) || !(0 <= j && j < dim)) throw new ArgumentException();
            if (i >= j) return x[Ind(i, j)];
            return 0;
        }
    }
}
