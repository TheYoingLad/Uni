using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Calculator.Model
{
    public class CalculatorEventArgs : EventArgs
    {
        private double _result;
        private string _calculationString;

        public double Result { get { return _result; } }
        public string CalculationString { get { return _calculationString; } }

        public CalculatorEventArgs(double result, string calculationString)
        {
            _result = result;
            _calculationString = calculationString;
        }
    }
}
