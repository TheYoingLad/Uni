namespace DigitalisOrak
{
    partial class Clock
    {
        /// <summary> 
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary> 
        /// Clean up any resources being used.
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

        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            cityLabel = new Label();
            timeLabel = new Label();
            SuspendLayout();
            // 
            // cityLabel
            // 
            cityLabel.AutoSize = true;
            cityLabel.Location = new Point(16, 13);
            cityLabel.Name = "cityLabel";
            cityLabel.Size = new Size(61, 15);
            cityLabel.TabIndex = 0;
            cityLabel.Text = "City name";
            // 
            // timeLabel
            // 
            timeLabel.BorderStyle = BorderStyle.Fixed3D;
            timeLabel.Font = new Font("Consolas", 15F, FontStyle.Regular, GraphicsUnit.Point, 238);
            timeLabel.Location = new Point(16, 28);
            timeLabel.Name = "timeLabel";
            timeLabel.Size = new Size(70, 29);
            timeLabel.TabIndex = 1;
            timeLabel.Text = "HH:MM";
            // 
            // Clock
            // 
            AutoScaleDimensions = new SizeF(7F, 15F);
            AutoScaleMode = AutoScaleMode.Font;
            Controls.Add(timeLabel);
            Controls.Add(cityLabel);
            Name = "Clock";
            Size = new Size(98, 69);
            ResumeLayout(false);
            PerformLayout();
        }

        #endregion

        private Label cityLabel;
        private Label timeLabel;
    }
}
