using Calculator.Model;

namespace Calculator
{
    public partial class CalculatorForm : Form
    {
        private CalculatorModel _model;

        public CalculatorForm()
        {
            InitializeComponent();

            _model = new CalculatorModel();
            _model.CalculationPerformed += new EventHandler<CalculatorEventArgs>(Model_CalculationPerformed);
            _textNumber.Text = _model.Result.ToString();
        }

        private void Model_CalculationPerformed(object? sender, CalculatorEventArgs e)
        {
            _textNumber.Text = e.Result.ToString();
            _listHisory.Items.Add(e.CalculationString);
            _textNumber.Focus();
            _textNumber.SelectAll();
        }
        private void Button_Click(object? sender, EventArgs e)
        {
            if (sender is Button b)
            {
                switch (b.Text)
                {
                    case "+": PerformCalculation(Operation.Add); break;
                    case "-": PerformCalculation(Operation.Substract); break;
                    case "*": PerformCalculation(Operation.Multiply); break;
                    case "/": PerformCalculation(Operation.Divide); break;
                    case "=": PerformCalculation(Operation.None); break;
                }
            }
        }
        private void CalculatorForm_KeyDown(object? sender, KeyEventArgs e)
        {
            switch (e.KeyCode)
            {
                case Keys.Add:
                    PerformCalculation(Operation.Add);
                    e.SuppressKeyPress = true;
                    break;
                case Keys.Subtract:
                    PerformCalculation(Operation.Substract);
                    e.SuppressKeyPress = true;
                    break;
                case Keys.Multiply:
                    PerformCalculation(Operation.Multiply);
                    e.SuppressKeyPress = true;
                    break;
                case Keys.Divide:
                    PerformCalculation(Operation.Divide);
                    e.SuppressKeyPress = true;
                    break;
                case Keys.Enter:
                    PerformCalculation(Operation.None);
                    e.SuppressKeyPress = true;
                    break;
            }
        }
        private void PerformCalculation(Operation op)
        {
            try
            {
                _model.Calculate(double.Parse(_textNumber.Text), op);
            }
            catch (OverflowException)
            {
                MessageBox.Show("Your input has too many digits!", "Calcualtion Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
            catch (FormatException)
            {
                MessageBox.Show("Your input is not a real number!\nPlease Correct!", "Calcualtion Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
            catch (NullReferenceException)
            {
                MessageBox.Show("No number in input!\nPlease correct!", "Calcualtion Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }
    }
}