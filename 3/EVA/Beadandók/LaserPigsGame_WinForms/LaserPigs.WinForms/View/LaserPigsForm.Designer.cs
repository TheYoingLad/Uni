namespace LaserPigs.View
{
    public partial class LaserPigsForm
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(LaserPigsForm));
            _menuStrip = new MenuStrip();
            _menuItemOptions = new ToolStripMenuItem();
            _menuItemNewGame = new ToolStripMenuItem();
            _menuItemX4 = new ToolStripMenuItem();
            _menuItemX6 = new ToolStripMenuItem();
            _menuItemX8 = new ToolStripMenuItem();
            toolStripSeparator1 = new ToolStripSeparator();
            _menuItemSaveGame = new ToolStripMenuItem();
            _menuItemLoadGame = new ToolStripMenuItem();
            toolStripSeparator2 = new ToolStripSeparator();
            _menuItemExit = new ToolStripMenuItem();
            _statusStrip = new StatusStrip();
            _statusStripLabelP1 = new ToolStripStatusLabel();
            _statusStripHealthBar1 = new ToolStripStatusLabel();
            toolStripStatusLabel5 = new ToolStripStatusLabel();
            _statusStripLabelP2 = new ToolStripStatusLabel();
            _statusStripHealthBar2 = new ToolStripStatusLabel();
            _buttonUp = new Button();
            _labelGamePhase = new Label();
            _buttonRight = new Button();
            _buttonLeft = new Button();
            _buttonDown = new Button();
            _bottonCW = new Button();
            _buttonCCW = new Button();
            _buttonShoot = new Button();
            _buttonPunch = new Button();
            _listBoxP1Moves = new ListBox();
            _buttonConfirm = new Button();
            _labelP1 = new Label();
            _listBoxP2Moves = new ListBox();
            labelPlaceholder = new Label();
            _labelP2 = new Label();
            _labelHowToPlay = new Label();
            _buttonDelete = new Button();
            _buttonNext = new Button();
            _labelP1Colour = new Label();
            _labelP2Colour = new Label();
            _labelInstructionsRemaining = new Label();
            _labelInstructionsRemainingValue = new Label();
            _menuStrip.SuspendLayout();
            _statusStrip.SuspendLayout();
            SuspendLayout();
            // 
            // _menuStrip
            // 
            _menuStrip.Items.AddRange(new ToolStripItem[] { _menuItemOptions });
            _menuStrip.Location = new Point(0, 0);
            _menuStrip.Name = "_menuStrip";
            _menuStrip.Size = new Size(1120, 24);
            _menuStrip.TabIndex = 1;
            _menuStrip.Text = "menuStrip1";
            // 
            // _menuItemOptions
            // 
            _menuItemOptions.DropDownItems.AddRange(new ToolStripItem[] { _menuItemNewGame, toolStripSeparator1, _menuItemSaveGame, _menuItemLoadGame, toolStripSeparator2, _menuItemExit });
            _menuItemOptions.Name = "_menuItemOptions";
            _menuItemOptions.Size = new Size(61, 20);
            _menuItemOptions.Text = "Options";
            // 
            // _menuItemNewGame
            // 
            _menuItemNewGame.DropDownItems.AddRange(new ToolStripItem[] { _menuItemX4, _menuItemX6, _menuItemX8 });
            _menuItemNewGame.Name = "_menuItemNewGame";
            _menuItemNewGame.Size = new Size(134, 22);
            _menuItemNewGame.Text = "New Game";
            // 
            // _menuItemX4
            // 
            _menuItemX4.CheckOnClick = true;
            _menuItemX4.Name = "_menuItemX4";
            _menuItemX4.Size = new Size(98, 22);
            _menuItemX4.Text = "4 x 4";
            _menuItemX4.Click += Menu_NewGame4X_Click;
            // 
            // _menuItemX6
            // 
            _menuItemX6.CheckOnClick = true;
            _menuItemX6.Name = "_menuItemX6";
            _menuItemX6.Size = new Size(98, 22);
            _menuItemX6.Text = "6 x 6";
            _menuItemX6.Click += Menu_NewGame6X_Click;
            // 
            // _menuItemX8
            // 
            _menuItemX8.CheckOnClick = true;
            _menuItemX8.Name = "_menuItemX8";
            _menuItemX8.Size = new Size(98, 22);
            _menuItemX8.Text = "8 x 8";
            _menuItemX8.Click += Menu_NewGame8X_Click;
            // 
            // toolStripSeparator1
            // 
            toolStripSeparator1.Name = "toolStripSeparator1";
            toolStripSeparator1.Size = new Size(131, 6);
            // 
            // _menuItemSaveGame
            // 
            _menuItemSaveGame.Name = "_menuItemSaveGame";
            _menuItemSaveGame.Size = new Size(134, 22);
            _menuItemSaveGame.Text = "Save Game";
            _menuItemSaveGame.Click += Menu_SaveGame_Click;
            // 
            // _menuItemLoadGame
            // 
            _menuItemLoadGame.Name = "_menuItemLoadGame";
            _menuItemLoadGame.Size = new Size(134, 22);
            _menuItemLoadGame.Text = "Load Game";
            _menuItemLoadGame.Click += Menu_LoadGame_Click;
            // 
            // toolStripSeparator2
            // 
            toolStripSeparator2.Name = "toolStripSeparator2";
            toolStripSeparator2.Size = new Size(131, 6);
            // 
            // _menuItemExit
            // 
            _menuItemExit.Name = "_menuItemExit";
            _menuItemExit.Size = new Size(134, 22);
            _menuItemExit.Text = "Exit";
            _menuItemExit.Click += Menu_Exit_Click;
            // 
            // _statusStrip
            // 
            _statusStrip.Items.AddRange(new ToolStripItem[] { _statusStripLabelP1, _statusStripHealthBar1, toolStripStatusLabel5, _statusStripLabelP2, _statusStripHealthBar2 });
            _statusStrip.Location = new Point(0, 708);
            _statusStrip.Name = "_statusStrip";
            _statusStrip.Size = new Size(1120, 22);
            _statusStrip.TabIndex = 2;
            _statusStrip.Text = "statusStrip1";
            // 
            // _statusStripLabelP1
            // 
            _statusStripLabelP1.Name = "_statusStripLabelP1";
            _statusStripLabelP1.Size = new Size(54, 17);
            _statusStripLabelP1.Text = "Player 1: ";
            // 
            // _statusStripHealthBar1
            // 
            _statusStripHealthBar1.Name = "_statusStripHealthBar1";
            _statusStripHealthBar1.Padding = new Padding(10, 0, 10, 0);
            _statusStripHealthBar1.Size = new Size(20, 17);
            _statusStripHealthBar1.TextAlign = ContentAlignment.MiddleLeft;
            // 
            // toolStripStatusLabel5
            // 
            toolStripStatusLabel5.Name = "toolStripStatusLabel5";
            toolStripStatusLabel5.Size = new Size(957, 17);
            toolStripStatusLabel5.Spring = true;
            // 
            // _statusStripLabelP2
            // 
            _statusStripLabelP2.Name = "_statusStripLabelP2";
            _statusStripLabelP2.Size = new Size(54, 17);
            _statusStripLabelP2.Text = "Player 2: ";
            // 
            // _statusStripHealthBar2
            // 
            _statusStripHealthBar2.Name = "_statusStripHealthBar2";
            _statusStripHealthBar2.Padding = new Padding(10, 0, 10, 0);
            _statusStripHealthBar2.Size = new Size(20, 17);
            _statusStripHealthBar2.TextAlign = ContentAlignment.MiddleLeft;
            // 
            // _buttonUp
            // 
            _buttonUp.Location = new Point(730, 77);
            _buttonUp.Name = "_buttonUp";
            _buttonUp.Size = new Size(80, 80);
            _buttonUp.TabIndex = 1;
            _buttonUp.Text = "UP";
            _buttonUp.UseVisualStyleBackColor = true;
            _buttonUp.Click += Button_Up_Click;
            // 
            // _labelGamePhase
            // 
            _labelGamePhase.Font = new Font("Segoe UI", 20.25F, FontStyle.Regular, GraphicsUnit.Point, 0);
            _labelGamePhase.Location = new Point(644, 34);
            _labelGamePhase.Name = "_labelGamePhase";
            _labelGamePhase.Size = new Size(252, 40);
            _labelGamePhase.TabIndex = 4;
            _labelGamePhase.Text = "_labelGamePhase";
            _labelGamePhase.TextAlign = ContentAlignment.MiddleCenter;
            // 
            // _buttonRight
            // 
            _buttonRight.Location = new Point(816, 163);
            _buttonRight.Name = "_buttonRight";
            _buttonRight.Size = new Size(80, 80);
            _buttonRight.TabIndex = 2;
            _buttonRight.Text = "RIGHT";
            _buttonRight.UseVisualStyleBackColor = true;
            _buttonRight.Click += Button_Right_Click;
            // 
            // _buttonLeft
            // 
            _buttonLeft.Location = new Point(644, 163);
            _buttonLeft.Name = "_buttonLeft";
            _buttonLeft.Size = new Size(80, 80);
            _buttonLeft.TabIndex = 4;
            _buttonLeft.Text = "LEFT";
            _buttonLeft.UseVisualStyleBackColor = true;
            _buttonLeft.Click += Button_Left_Click;
            // 
            // _buttonDown
            // 
            _buttonDown.Location = new Point(730, 249);
            _buttonDown.Name = "_buttonDown";
            _buttonDown.Size = new Size(80, 80);
            _buttonDown.TabIndex = 3;
            _buttonDown.Text = "DOWN";
            _buttonDown.UseVisualStyleBackColor = true;
            _buttonDown.Click += Button_Down_Click;
            // 
            // _bottonCW
            // 
            _bottonCW.Location = new Point(1023, 77);
            _bottonCW.Name = "_bottonCW";
            _bottonCW.Size = new Size(80, 80);
            _bottonCW.TabIndex = 6;
            _bottonCW.Text = "ROTATE CW";
            _bottonCW.UseVisualStyleBackColor = true;
            _bottonCW.Click += Button_CW_Click;
            // 
            // _buttonCCW
            // 
            _buttonCCW.Location = new Point(937, 77);
            _buttonCCW.Name = "_buttonCCW";
            _buttonCCW.Size = new Size(80, 80);
            _buttonCCW.TabIndex = 5;
            _buttonCCW.Text = "ROTATE CCW";
            _buttonCCW.UseVisualStyleBackColor = true;
            _buttonCCW.Click += Button_CCW_Click;
            // 
            // _buttonShoot
            // 
            _buttonShoot.Location = new Point(1023, 249);
            _buttonShoot.Name = "_buttonShoot";
            _buttonShoot.Size = new Size(80, 80);
            _buttonShoot.TabIndex = 8;
            _buttonShoot.Text = "SHOOT";
            _buttonShoot.UseVisualStyleBackColor = true;
            _buttonShoot.Click += Button_Shoot_Click;
            // 
            // _buttonPunch
            // 
            _buttonPunch.Location = new Point(937, 249);
            _buttonPunch.Name = "_buttonPunch";
            _buttonPunch.Size = new Size(80, 80);
            _buttonPunch.TabIndex = 7;
            _buttonPunch.Text = "PUNCH";
            _buttonPunch.UseVisualStyleBackColor = true;
            _buttonPunch.Click += Button_Punch_Click;
            // 
            // _listBoxP1Moves
            // 
            _listBoxP1Moves.FormattingEnabled = true;
            _listBoxP1Moves.ItemHeight = 15;
            _listBoxP1Moves.Location = new Point(644, 394);
            _listBoxP1Moves.Name = "_listBoxP1Moves";
            _listBoxP1Moves.Size = new Size(110, 169);
            _listBoxP1Moves.TabIndex = 14;
            _listBoxP1Moves.TabStop = false;
            // 
            // _buttonConfirm
            // 
            _buttonConfirm.Location = new Point(644, 614);
            _buttonConfirm.Name = "_buttonConfirm";
            _buttonConfirm.Size = new Size(110, 75);
            _buttonConfirm.TabIndex = 9;
            _buttonConfirm.Text = "CONFIRM";
            _buttonConfirm.UseVisualStyleBackColor = true;
            _buttonConfirm.Click += Button_Confirm_Click;
            // 
            // _labelP1
            // 
            _labelP1.AutoSize = true;
            _labelP1.Location = new Point(675, 366);
            _labelP1.Name = "_labelP1";
            _labelP1.Size = new Size(48, 15);
            _labelP1.TabIndex = 20;
            _labelP1.Text = "Player 1";
            _labelP1.TextAlign = ContentAlignment.MiddleCenter;
            // 
            // _listBoxP2Moves
            // 
            _listBoxP2Moves.FormattingEnabled = true;
            _listBoxP2Moves.ItemHeight = 15;
            _listBoxP2Moves.Location = new Point(771, 394);
            _listBoxP2Moves.Name = "_listBoxP2Moves";
            _listBoxP2Moves.Size = new Size(110, 169);
            _listBoxP2Moves.TabIndex = 23;
            _listBoxP2Moves.TabStop = false;
            // 
            // labelPlaceholder
            // 
            labelPlaceholder.BorderStyle = BorderStyle.FixedSingle;
            labelPlaceholder.Font = new Font("Segoe UI", 40F);
            labelPlaceholder.Location = new Point(20, 64);
            labelPlaceholder.Name = "labelPlaceholder";
            labelPlaceholder.Size = new Size(600, 600);
            labelPlaceholder.TabIndex = 25;
            labelPlaceholder.Text = "PLACEHOLDER";
            labelPlaceholder.TextAlign = ContentAlignment.MiddleCenter;
            labelPlaceholder.Visible = false;
            // 
            // _labelP2
            // 
            _labelP2.AutoSize = true;
            _labelP2.Location = new Point(802, 366);
            _labelP2.Name = "_labelP2";
            _labelP2.Size = new Size(48, 15);
            _labelP2.TabIndex = 27;
            _labelP2.Text = "Player 2";
            _labelP2.TextAlign = ContentAlignment.MiddleCenter;
            // 
            // _labelHowToPlay
            // 
            _labelHowToPlay.BorderStyle = BorderStyle.Fixed3D;
            _labelHowToPlay.Location = new Point(937, 394);
            _labelHowToPlay.Name = "_labelHowToPlay";
            _labelHowToPlay.Size = new Size(166, 295);
            _labelHowToPlay.TabIndex = 28;
            _labelHowToPlay.Text = resources.GetString("_labelHowToPlay.Text");
            // 
            // _buttonDelete
            // 
            _buttonDelete.Location = new Point(730, 163);
            _buttonDelete.Name = "_buttonDelete";
            _buttonDelete.Size = new Size(80, 80);
            _buttonDelete.TabIndex = 0;
            _buttonDelete.Text = "DELETE";
            _buttonDelete.UseVisualStyleBackColor = true;
            _buttonDelete.Click += Button_Delete_Click;
            // 
            // _buttonNext
            // 
            _buttonNext.Location = new Point(771, 614);
            _buttonNext.Name = "_buttonNext";
            _buttonNext.Size = new Size(110, 75);
            _buttonNext.TabIndex = 10;
            _buttonNext.Text = "NEXT";
            _buttonNext.UseVisualStyleBackColor = true;
            _buttonNext.Click += Button_Next_Click;
            // 
            // _labelP1Colour
            // 
            _labelP1Colour.AutoSize = true;
            _labelP1Colour.Location = new Point(651, 366);
            _labelP1Colour.Name = "_labelP1Colour";
            _labelP1Colour.Size = new Size(18, 15);
            _labelP1Colour.TabIndex = 29;
            _labelP1Colour.Text = "█";
            // 
            // _labelP2Colour
            // 
            _labelP2Colour.AutoSize = true;
            _labelP2Colour.Location = new Point(778, 366);
            _labelP2Colour.Name = "_labelP2Colour";
            _labelP2Colour.Size = new Size(18, 15);
            _labelP2Colour.TabIndex = 30;
            _labelP2Colour.Text = "█";
            // 
            // _labelInstructionsRemaining
            // 
            _labelInstructionsRemaining.AutoSize = true;
            _labelInstructionsRemaining.Location = new Point(644, 581);
            _labelInstructionsRemaining.Name = "_labelInstructionsRemaining";
            _labelInstructionsRemaining.Size = new Size(129, 15);
            _labelInstructionsRemaining.TabIndex = 32;
            _labelInstructionsRemaining.Text = "Instructions remaining:";
            // 
            // _labelInstructionsRemainingValue
            // 
            _labelInstructionsRemainingValue.AutoSize = true;
            _labelInstructionsRemainingValue.Location = new Point(779, 581);
            _labelInstructionsRemainingValue.Name = "_labelInstructionsRemainingValue";
            _labelInstructionsRemainingValue.Size = new Size(35, 15);
            _labelInstructionsRemainingValue.TabIndex = 33;
            _labelInstructionsRemainingValue.Text = "value";
            // 
            // LaserPigsForm
            // 
            AutoScaleDimensions = new SizeF(7F, 15F);
            AutoScaleMode = AutoScaleMode.Font;
            ClientSize = new Size(1120, 730);
            Controls.Add(_labelInstructionsRemainingValue);
            Controls.Add(_labelInstructionsRemaining);
            Controls.Add(_labelP2Colour);
            Controls.Add(_labelP1Colour);
            Controls.Add(_buttonNext);
            Controls.Add(_buttonDelete);
            Controls.Add(_labelHowToPlay);
            Controls.Add(_labelP2);
            Controls.Add(labelPlaceholder);
            Controls.Add(_listBoxP2Moves);
            Controls.Add(_labelP1);
            Controls.Add(_buttonConfirm);
            Controls.Add(_listBoxP1Moves);
            Controls.Add(_buttonPunch);
            Controls.Add(_buttonShoot);
            Controls.Add(_buttonCCW);
            Controls.Add(_bottonCW);
            Controls.Add(_buttonDown);
            Controls.Add(_buttonLeft);
            Controls.Add(_buttonRight);
            Controls.Add(_labelGamePhase);
            Controls.Add(_buttonUp);
            Controls.Add(_statusStrip);
            Controls.Add(_menuStrip);
            FormBorderStyle = FormBorderStyle.FixedSingle;
            MainMenuStrip = _menuStrip;
            MaximizeBox = false;
            Name = "LaserPigsForm";
            Text = "Laser Pigs";
            _menuStrip.ResumeLayout(false);
            _menuStrip.PerformLayout();
            _statusStrip.ResumeLayout(false);
            _statusStrip.PerformLayout();
            ResumeLayout(false);
            PerformLayout();
        }

        #endregion

        private MenuStrip _menuStrip;
        private StatusStrip _statusStrip;
        private ToolStripMenuItem _menuItemOptions;
        private ToolStripMenuItem _menuItemNewGame;
        private ToolStripSeparator toolStripSeparator1;
        private ToolStripMenuItem _menuItemSaveGame;
        private ToolStripMenuItem _menuItemX4;
        private ToolStripMenuItem _menuItemX6;
        private ToolStripMenuItem _menuItemX8;
        private ToolStripMenuItem _menuItemLoadGame;
        private ToolStripSeparator toolStripSeparator2;
        private ToolStripMenuItem _menuItemExit;
        private ToolStripStatusLabel _statusStripLabelP1;
        private ToolStripStatusLabel _statusStripHealthBar1;
        private ToolStripStatusLabel _statusStripLabelP2;
        private ToolStripStatusLabel _statusStripHealthBar2;
        private ToolStripStatusLabel toolStripStatusLabel5;
        private Button _buttonUp;
        private Label _labelGamePhase;
        private Button _buttonRight;
        private Button _buttonLeft;
        private Button _buttonDown;
        private Button _bottonCW;
        private Button _buttonCCW;
        private Button _buttonShoot;
        private Button _buttonPunch;
        private ListBox _listBoxP1Moves;
        private Button _buttonConfirm;
        private Label _labelP1;
        private ListBox _listBoxP2Moves;
        private Label labelPlaceholder;
        private Label _labelP2;
        private Label _labelHowToPlay;
        private Button _buttonDelete;
        private Button _buttonNext;
        private Label _labelP1Colour;
        private Label _labelP2Colour;
        private Label _labelInstructionsRemaining;
        private Label _labelInstructionsRemainingValue;
    }
}
