namespace Grid
{
    partial class GridForm
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
            _numericRows = new NumericUpDown();
            _tableLayoutGrid = new TableLayoutPanel();
            _buttonSize = new Button();
            _groupBox = new GroupBox();
            _numericColumns = new NumericUpDown();
            label2 = new Label();
            label1 = new Label();
            ((System.ComponentModel.ISupportInitialize)_numericRows).BeginInit();
            _groupBox.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)_numericColumns).BeginInit();
            SuspendLayout();
            // 
            // _numericRows
            // 
            _numericRows.Location = new Point(52, 22);
            _numericRows.Maximum = new decimal(new int[] { 20, 0, 0, 0 });
            _numericRows.Minimum = new decimal(new int[] { 1, 0, 0, 0 });
            _numericRows.Name = "_numericRows";
            _numericRows.Size = new Size(120, 23);
            _numericRows.TabIndex = 0;
            _numericRows.Value = new decimal(new int[] { 1, 0, 0, 0 });
            // 
            // _tableLayoutGrid
            // 
            _tableLayoutGrid.AutoSizeMode = AutoSizeMode.GrowAndShrink;
            _tableLayoutGrid.ColumnCount = 1;
            _tableLayoutGrid.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 50F));
            _tableLayoutGrid.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 50F));
            _tableLayoutGrid.GrowStyle = TableLayoutPanelGrowStyle.FixedSize;
            _tableLayoutGrid.Location = new Point(12, 76);
            _tableLayoutGrid.Name = "_tableLayoutGrid";
            _tableLayoutGrid.RowCount = 1;
            _tableLayoutGrid.RowStyles.Add(new RowStyle(SizeType.Percent, 50F));
            _tableLayoutGrid.RowStyles.Add(new RowStyle(SizeType.Percent, 50F));
            _tableLayoutGrid.Size = new Size(837, 421);
            _tableLayoutGrid.TabIndex = 1;
            // 
            // _buttonSize
            // 
            _buttonSize.ForeColor = SystemColors.WindowText;
            _buttonSize.Location = new Point(481, 20);
            _buttonSize.Name = "_buttonSize";
            _buttonSize.Size = new Size(112, 23);
            _buttonSize.TabIndex = 2;
            _buttonSize.Text = "Méretváltás";
            _buttonSize.UseVisualStyleBackColor = true;
            _buttonSize.Click += ButtonSize_Click;
            // 
            // _groupBox
            // 
            _groupBox.Controls.Add(_numericColumns);
            _groupBox.Controls.Add(label2);
            _groupBox.Controls.Add(label1);
            _groupBox.Controls.Add(_buttonSize);
            _groupBox.Controls.Add(_numericRows);
            _groupBox.Location = new Point(12, 12);
            _groupBox.Name = "_groupBox";
            _groupBox.Size = new Size(615, 58);
            _groupBox.TabIndex = 3;
            _groupBox.TabStop = false;
            _groupBox.Text = "Méret:";
            // 
            // _numericColumns
            // 
            _numericColumns.Location = new Point(289, 22);
            _numericColumns.Maximum = new decimal(new int[] { 20, 0, 0, 0 });
            _numericColumns.Minimum = new decimal(new int[] { 1, 0, 0, 0 });
            _numericColumns.Name = "_numericColumns";
            _numericColumns.Size = new Size(120, 23);
            _numericColumns.TabIndex = 5;
            _numericColumns.Value = new decimal(new int[] { 1, 0, 0, 0 });
            // 
            // label2
            // 
            label2.AutoSize = true;
            label2.Location = new Point(224, 26);
            label2.Name = "label2";
            label2.Size = new Size(59, 15);
            label2.TabIndex = 4;
            label2.Text = "Oszlopok:";
            // 
            // label1
            // 
            label1.AutoSize = true;
            label1.Location = new Point(6, 26);
            label1.Name = "label1";
            label1.Size = new Size(40, 15);
            label1.TabIndex = 3;
            label1.Text = "Sorok:";
            // 
            // GridForm
            // 
            AutoScaleDimensions = new SizeF(7F, 15F);
            AutoScaleMode = AutoScaleMode.Font;
            ClientSize = new Size(861, 509);
            Controls.Add(_groupBox);
            Controls.Add(_tableLayoutGrid);
            Name = "GridForm";
            Text = "Grid";
            ((System.ComponentModel.ISupportInitialize)_numericRows).EndInit();
            _groupBox.ResumeLayout(false);
            _groupBox.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)_numericColumns).EndInit();
            ResumeLayout(false);
        }

        #endregion

        private NumericUpDown _numericRows;
        private TableLayoutPanel _tableLayoutGrid;
        private Button _buttonSize;
        private GroupBox _groupBox;
        private Label label1;
        private Label label2;
        private NumericUpDown _numericColumns;
    }
}
