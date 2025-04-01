using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Calculator.Model
{
    public enum Operation
    { None, Add, Substract, Multiply, Divide }

    public class CalculatorModel
    {
        private double _result;
        private Operation _operation;

        public double Result { get { return _result; } }

        public CalculatorModel()
        {
            _operation = Operation.None;
            _result = 0;
        }

        public void Calculate(double val, Operation op)
        {
            string calculationString = "";
            if (_operation != Operation.None)
            {
                switch (_operation)
                {
                    case Operation.Add:
                        calculationString = _result + " + " + val + " = " + (_result + val);
                        _result += val;
                        break;
                    case Operation.Substract:
                        calculationString = _result + " - " + val + " = " + (_result - val);
                        _result -= val;
                        break;
                    case Operation.Multiply:
                        calculationString = _result + " * " + val + " = " + (_result * val);
                        _result *= val;
                        break;
                    case Operation.Divide:
                        calculationString = _result + " / " + val + " = " + (_result / val);
                        _result /= val;
                        break;
                }
            }
            else
            {
                _result = val;
            }
            _operation = op;
            OnCalculationPerformed(calculationString);
        }
        private void OnCalculationPerformed(string calculationString)
        {
            CalculationPerformed?.Invoke(this, new CalculatorEventArgs(_result, calculationString));
        }

        public event EventHandler<CalculatorEventArgs>? CalculationPerformed;
    }
}
