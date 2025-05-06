namespace Snake.View
{
    partial class Form
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Form));
            MenuBox = new GroupBox();
            ScoreLabel = new Label();
            GameOverLabel = new Label();
            loadedLabel = new Label();
            ExitBtn = new Button();
            LoadMapBtn = new Button();
            StartBtn = new Button();
            MenuBox.SuspendLayout();
            SuspendLayout();
            // 
            // MenuBox
            // 
            MenuBox.BackColor = Color.Silver;
            MenuBox.Controls.Add(ScoreLabel);
            MenuBox.Controls.Add(GameOverLabel);
            MenuBox.Controls.Add(loadedLabel);
            MenuBox.Controls.Add(ExitBtn);
            MenuBox.Controls.Add(LoadMapBtn);
            MenuBox.Controls.Add(StartBtn);
            MenuBox.Font = new Font("Impact", 27.8571434F, FontStyle.Regular, GraphicsUnit.Point, 238);
            MenuBox.Location = new Point(238, 219);
            MenuBox.Name = "MenuBox";
            MenuBox.Size = new Size(750, 750);
            MenuBox.TabIndex = 0;
            MenuBox.TabStop = false;
            MenuBox.Text = "Menu";
            // 
            // ScoreLabel
            // 
            ScoreLabel.AutoSize = true;
            ScoreLabel.Font = new Font("Impact", 15.8571434F, FontStyle.Regular, GraphicsUnit.Point, 238);
            ScoreLabel.ForeColor = Color.Navy;
            ScoreLabel.Location = new Point(280, 175);
            ScoreLabel.Name = "ScoreLabel";
            ScoreLabel.Size = new Size(182, 45);
            ScoreLabel.TabIndex = 5;
            ScoreLabel.Text = "Score: 000";
            ScoreLabel.Visible = false;
            // 
            // GameOverLabel
            // 
            GameOverLabel.Font = new Font("Impact", 24F, FontStyle.Regular, GraphicsUnit.Point, 238);
            GameOverLabel.ForeColor = Color.Red;
            GameOverLabel.Location = new Point(239, 83);
            GameOverLabel.Name = "GameOverLabel";
            GameOverLabel.Size = new Size(273, 68);
            GameOverLabel.TabIndex = 4;
            GameOverLabel.TextAlign = ContentAlignment.MiddleCenter;
            // 
            // loadedLabel
            // 
            loadedLabel.AutoSize = true;
            loadedLabel.Font = new Font("Impact", 14.1428576F, FontStyle.Italic, GraphicsUnit.Point, 238);
            loadedLabel.ForeColor = Color.FromArgb(0, 48, 0);
            loadedLabel.Location = new Point(371, 706);
            loadedLabel.Name = "loadedLabel";
            loadedLabel.Size = new Size(379, 41);
            loadedLabel.TabIndex = 3;
            loadedLabel.Text = "A map has been loaded in...";
            loadedLabel.Visible = false;
            // 
            // ExitBtn
            // 
            ExitBtn.BackColor = Color.LightSkyBlue;
            ExitBtn.Font = new Font("Impact", 21.8571434F, FontStyle.Regular, GraphicsUnit.Point, 238);
            ExitBtn.ForeColor = Color.DarkMagenta;
            ExitBtn.Location = new Point(225, 500);
            ExitBtn.Name = "ExitBtn";
            ExitBtn.Size = new Size(300, 100);
            ExitBtn.TabIndex = 2;
            ExitBtn.Text = "Exit";
            ExitBtn.UseVisualStyleBackColor = false;
            ExitBtn.Click += ExitBtn_Click;
            // 
            // LoadMapBtn
            // 
            LoadMapBtn.BackColor = Color.LightSalmon;
            LoadMapBtn.Font = new Font("Impact", 21.8571434F, FontStyle.Regular, GraphicsUnit.Point, 238);
            LoadMapBtn.ForeColor = Color.FromArgb(0, 64, 0);
            LoadMapBtn.Location = new Point(225, 385);
            LoadMapBtn.Name = "LoadMapBtn";
            LoadMapBtn.Size = new Size(300, 100);
            LoadMapBtn.TabIndex = 1;
            LoadMapBtn.Text = "Load Map";
            LoadMapBtn.UseVisualStyleBackColor = false;
            LoadMapBtn.Click += LoadMapBtn_Click;
            // 
            // StartBtn
            // 
            StartBtn.BackColor = Color.RosyBrown;
            StartBtn.Font = new Font("Impact", 21.8571434F, FontStyle.Regular, GraphicsUnit.Point, 238);
            StartBtn.ForeColor = Color.DarkRed;
            StartBtn.Location = new Point(225, 270);
            StartBtn.Name = "StartBtn";
            StartBtn.Size = new Size(300, 100);
            StartBtn.TabIndex = 0;
            StartBtn.Text = "Play";
            StartBtn.UseVisualStyleBackColor = false;
            StartBtn.Click += StartBtn_Click;
            // 
            // Form
            // 
            AutoScaleDimensions = new SizeF(12F, 30F);
            AutoScaleMode = AutoScaleMode.Font;
            BackColor = Color.White;
            ClientSize = new Size(1226, 1186);
            Controls.Add(MenuBox);
            ForeColor = Color.Black;
            Icon = (Icon)resources.GetObject("$this.Icon");
            Margin = new Padding(3, 2, 3, 2);
            MaximizeBox = false;
            MaximumSize = new Size(1250, 1250);
            MinimumSize = new Size(1250, 1250);
            Name = "Form";
            StartPosition = FormStartPosition.CenterScreen;
            Text = "Snake";
            KeyDown += Form_KeyDown;
            MenuBox.ResumeLayout(false);
            MenuBox.PerformLayout();
            ResumeLayout(false);
        }

        #endregion

        private GroupBox MenuBox;
        private Button StartBtn;
        private Button LoadMapBtn;
        private Button ExitBtn;
        private Label loadedLabel;
        private Label GameOverLabel;
        private Label ScoreLabel;
    }
}
