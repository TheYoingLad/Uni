namespace Minefield.WinForms
{
    partial class GameForm
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
            _statusStrip = new StatusStrip();
            statusLabel = new ToolStripStatusLabel();
            _pauseMenuStrip = new MenuStrip();
            _pauseMenu = new ToolStripMenuItem();
            _menuNewGame = new ToolStripMenuItem();
            _menuSaveGame = new ToolStripMenuItem();
            _menuLoadGame = new ToolStripMenuItem();
            _saveFileDialog = new SaveFileDialog();
            _openFileDialog = new OpenFileDialog();
            _statusStrip.SuspendLayout();
            _pauseMenuStrip.SuspendLayout();
            SuspendLayout();
            // 
            // _statusStrip
            // 
            _statusStrip.Items.AddRange(new ToolStripItem[] { statusLabel });
            _statusStrip.Location = new Point(0, 428);
            _statusStrip.Name = "_statusStrip";
            _statusStrip.Size = new Size(800, 22);
            _statusStrip.TabIndex = 0;
            _statusStrip.Text = "statusStrip1";
            // 
            // statusLabel
            // 
            statusLabel.Name = "statusLabel";
            statusLabel.Size = new Size(0, 17);
            // 
            // _pauseMenuStrip
            // 
            _pauseMenuStrip.Items.AddRange(new ToolStripItem[] { _pauseMenu });
            _pauseMenuStrip.Location = new Point(0, 0);
            _pauseMenuStrip.Name = "_pauseMenuStrip";
            _pauseMenuStrip.Size = new Size(800, 24);
            _pauseMenuStrip.TabIndex = 1;
            _pauseMenuStrip.Text = "menuStrip1";
            // 
            // _pauseMenu
            // 
            _pauseMenu.DropDownItems.AddRange(new ToolStripItem[] { _menuNewGame, _menuSaveGame, _menuLoadGame });
            _pauseMenu.Name = "_pauseMenu";
            _pauseMenu.Size = new Size(50, 20);
            _pauseMenu.Text = "Menu";
            // 
            // _menuNewGame
            // 
            _menuNewGame.Name = "_menuNewGame";
            _menuNewGame.Size = new Size(180, 22);
            _menuNewGame.Text = "New Game";
            _menuNewGame.Click += MenuNewGame_Click;
            // 
            // _menuSaveGame
            // 
            _menuSaveGame.Name = "_menuSaveGame";
            _menuSaveGame.Size = new Size(180, 22);
            _menuSaveGame.Text = "Save Game";
            _menuSaveGame.Click += MenuSaveGame_Click;
            // 
            // _menuLoadGame
            // 
            _menuLoadGame.Name = "_menuLoadGame";
            _menuLoadGame.Size = new Size(180, 22);
            _menuLoadGame.Text = "Load Game";
            _menuLoadGame.Click += MenuLoadGame_Click;
            // 
            // _saveFileDialog
            // 
            _saveFileDialog.Filter = "Minefield savefile (*.mfs)|*.mfs";
            // 
            // _openFileDialog
            // 
            _openFileDialog.Filter = "Minefield savefile (*.mfs)|*.mfs";
            // 
            // GameForm
            // 
            AutoScaleDimensions = new SizeF(7F, 15F);
            AutoScaleMode = AutoScaleMode.Font;
            ClientSize = new Size(800, 450);
            Controls.Add(_statusStrip);
            Controls.Add(_pauseMenuStrip);
            MainMenuStrip = _pauseMenuStrip;
            Name = "GameForm";
            Text = "Minefield";
            FormClosing += GameForm_FormClosing;
            KeyDown += GameForm_KeyDown;
            _statusStrip.ResumeLayout(false);
            _statusStrip.PerformLayout();
            _pauseMenuStrip.ResumeLayout(false);
            _pauseMenuStrip.PerformLayout();
            ResumeLayout(false);
            PerformLayout();
        }

        #endregion

        private StatusStrip _statusStrip;
        private ToolStripStatusLabel statusLabel;
        private MenuStrip _pauseMenuStrip;
        private ToolStripMenuItem _pauseMenu;
        private ToolStripMenuItem _menuNewGame;
        private ToolStripMenuItem _menuSaveGame;
        private ToolStripMenuItem _menuLoadGame;
        private SaveFileDialog _saveFileDialog;
        private OpenFileDialog _openFileDialog;
    }
}
