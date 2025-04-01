namespace Calculator
{
    partial class CalculatorForm
    {
        /// <summary>
        ///  Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        ///  Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        ///  Required method for Designer support - do not modify
        ///  the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            _textNumber = new TextBox();
            _buttonAdd = new Button();
            _listHisory = new ListBox();
            _buttonSub = new Button();
            _buttonMul = new Button();
            _buttonDiv = new Button();
            _buttonEq = new Button();
            SuspendLayout();
            // 
            // _textNumber
            // 
            _textNumber.Font = new Font("Segoe UI", 15F);
            _textNumber.Location = new Point(12, 12);
            _textNumber.Name = "_textNumber";
            _textNumber.Size = new Size(328, 34);
            _textNumber.TabIndex = 0;
            // 
            // _buttonAdd
            // 
            _buttonAdd.Font = new Font("Segoe UI", 30F, FontStyle.Bold);
            _buttonAdd.Location = new Point(12, 52);
            _buttonAdd.Name = "_buttonAdd";
            _buttonAdd.Size = new Size(70, 70);
            _buttonAdd.TabIndex = 1;
            _buttonAdd.Text = "+";
            _buttonAdd.UseVisualStyleBackColor = true;
            _buttonAdd.Click += Button_Click;
            // 
            // _listHisory
            // 
            _listHisory.FormattingEnabled = true;
            _listHisory.ItemHeight = 15;
            _listHisory.Location = new Point(177, 52);
            _listHisory.Name = "_listHisory";
            _listHisory.Size = new Size(163, 229);
            _listHisory.TabIndex = 6;
            // 
            // _buttonSub
            // 
            _buttonSub.Font = new Font("Segoe UI", 30F, FontStyle.Bold);
            _buttonSub.Location = new Point(88, 52);
            _buttonSub.Name = "_buttonSub";
            _buttonSub.Size = new Size(70, 70);
            _buttonSub.TabIndex = 7;
            _buttonSub.Text = "-";
            _buttonSub.UseVisualStyleBackColor = true;
            _buttonSub.Click += Button_Click;
            // 
            // _buttonMul
            // 
            _buttonMul.Font = new Font("Segoe UI", 30F, FontStyle.Bold);
            _buttonMul.Location = new Point(12, 128);
            _buttonMul.Name = "_buttonMul";
            _buttonMul.Size = new Size(70, 70);
            _buttonMul.TabIndex = 8;
            _buttonMul.Text = "*";
            _buttonMul.UseVisualStyleBackColor = true;
            _buttonMul.Click += Button_Click;
            // 
            // _buttonDiv
            // 
            _buttonDiv.Font = new Font("Segoe UI", 30F, FontStyle.Bold);
            _buttonDiv.Location = new Point(88, 128);
            _buttonDiv.Name = "_buttonDiv";
            _buttonDiv.Size = new Size(70, 70);
            _buttonDiv.TabIndex = 9;
            _buttonDiv.Text = "/";
            _buttonDiv.UseVisualStyleBackColor = true;
            _buttonDiv.Click += Button_Click;
            // 
            // _buttonEq
            // 
            _buttonEq.Font = new Font("Segoe UI", 30F, FontStyle.Bold);
            _buttonEq.Location = new Point(12, 211);
            _buttonEq.Name = "_buttonEq";
            _buttonEq.Size = new Size(146, 70);
            _buttonEq.TabIndex = 10;
            _buttonEq.Text = "=";
            _buttonEq.UseVisualStyleBackColor = true;
            _buttonEq.Click += Button_Click;
            // 
            // CalculatorForm
            // 
            AutoScaleDimensions = new SizeF(7F, 15F);
            AutoScaleMode = AutoScaleMode.Font;
            ClientSize = new Size(360, 313);
            Controls.Add(_buttonEq);
            Controls.Add(_buttonDiv);
            Controls.Add(_buttonMul);
            Controls.Add(_buttonSub);
            Controls.Add(_listHisory);
            Controls.Add(_buttonAdd);
            Controls.Add(_textNumber);
            KeyPreview = true;
            Name = "CalculatorForm";
            Text = "Calculator";
            KeyDown += CalculatorForm_KeyDown;
            ResumeLayout(false);
            PerformLayout();
        }

        #endregion

        private TextBox _textNumber;
        private Button _buttonAdd;
        private ListBox _listHisory;
        private Button _buttonSub;
        private Button _buttonMul;
        private Button _buttonDiv;
        private Button _buttonEq;
    }
}
