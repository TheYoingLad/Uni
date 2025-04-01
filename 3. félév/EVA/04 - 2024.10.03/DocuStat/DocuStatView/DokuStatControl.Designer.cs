namespace DocuStatView
{
    partial class DokuStatControl
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
            textBoxIgnoredWords = new TextBox();
            spinBoxMinOccurrence = new NumericUpDown();
            spinBoxMinLength = new NumericUpDown();
            labelCharacters = new Label();
            labelNonWhitespaceCharacters = new Label();
            labelSentences = new Label();
            labelProperNouns = new Label();
            labelColemanLieuIndex = new Label();
            labelFleschReadingEase = new Label();
            labelMinWordOccurence = new Label();
            labelIgnoredWords = new Label();
            labelMinWordLength = new Label();
            listBoxCounter = new ListBox();
            textBox = new TextBox();
            ((System.ComponentModel.ISupportInitialize)spinBoxMinOccurrence).BeginInit();
            ((System.ComponentModel.ISupportInitialize)spinBoxMinLength).BeginInit();
            SuspendLayout();
            // 
            // textBoxIgnoredWords
            // 
            textBoxIgnoredWords.Location = new Point(594, 390);
            textBoxIgnoredWords.Name = "textBoxIgnoredWords";
            textBoxIgnoredWords.Size = new Size(158, 23);
            textBoxIgnoredWords.TabIndex = 28;
            // 
            // spinBoxMinOccurrence
            // 
            spinBoxMinOccurrence.Location = new Point(676, 348);
            spinBoxMinOccurrence.Maximum = new decimal(new int[] { 500, 0, 0, 0 });
            spinBoxMinOccurrence.Minimum = new decimal(new int[] { 1, 0, 0, 0 });
            spinBoxMinOccurrence.Name = "spinBoxMinOccurrence";
            spinBoxMinOccurrence.Size = new Size(76, 23);
            spinBoxMinOccurrence.TabIndex = 27;
            spinBoxMinOccurrence.Value = new decimal(new int[] { 1, 0, 0, 0 });
            // 
            // spinBoxMinLength
            // 
            spinBoxMinLength.Location = new Point(676, 304);
            spinBoxMinLength.Maximum = new decimal(new int[] { 500, 0, 0, 0 });
            spinBoxMinLength.Minimum = new decimal(new int[] { 1, 0, 0, 0 });
            spinBoxMinLength.Name = "spinBoxMinLength";
            spinBoxMinLength.Size = new Size(76, 23);
            spinBoxMinLength.TabIndex = 26;
            spinBoxMinLength.Value = new decimal(new int[] { 1, 0, 0, 0 });
            // 
            // labelCharacters
            // 
            labelCharacters.AutoSize = true;
            labelCharacters.Location = new Point(3, 306);
            labelCharacters.Name = "labelCharacters";
            labelCharacters.Size = new Size(95, 15);
            labelCharacters.TabIndex = 25;
            labelCharacters.Text = "Character count:";
            // 
            // labelNonWhitespaceCharacters
            // 
            labelNonWhitespaceCharacters.AutoSize = true;
            labelNonWhitespaceCharacters.Location = new Point(3, 350);
            labelNonWhitespaceCharacters.Name = "labelNonWhitespaceCharacters";
            labelNonWhitespaceCharacters.Size = new Size(154, 15);
            labelNonWhitespaceCharacters.TabIndex = 24;
            labelNonWhitespaceCharacters.Text = "Non-whitespace characters:";
            // 
            // labelSentences
            // 
            labelSentences.AutoSize = true;
            labelSentences.Location = new Point(3, 393);
            labelSentences.Name = "labelSentences";
            labelSentences.Size = new Size(92, 15);
            labelSentences.TabIndex = 23;
            labelSentences.Text = "Sentence count:";
            // 
            // labelProperNouns
            // 
            labelProperNouns.AutoSize = true;
            labelProperNouns.Location = new Point(254, 306);
            labelProperNouns.Name = "labelProperNouns";
            labelProperNouns.Size = new Size(110, 15);
            labelProperNouns.TabIndex = 22;
            labelProperNouns.Text = "Proper noun count:";
            // 
            // labelColemanLieuIndex
            // 
            labelColemanLieuIndex.AutoSize = true;
            labelColemanLieuIndex.Location = new Point(254, 350);
            labelColemanLieuIndex.Name = "labelColemanLieuIndex";
            labelColemanLieuIndex.Size = new Size(115, 15);
            labelColemanLieuIndex.TabIndex = 21;
            labelColemanLieuIndex.Text = "Coleman Lieu Index:";
            // 
            // labelFleschReadingEase
            // 
            labelFleschReadingEase.AutoSize = true;
            labelFleschReadingEase.Location = new Point(254, 393);
            labelFleschReadingEase.Name = "labelFleschReadingEase";
            labelFleschReadingEase.Size = new Size(115, 15);
            labelFleschReadingEase.TabIndex = 20;
            labelFleschReadingEase.Text = "Flesch Reading Ease:";
            // 
            // labelMinWordOccurence
            // 
            labelMinWordOccurence.AutoSize = true;
            labelMinWordOccurence.Location = new Point(498, 350);
            labelMinWordOccurence.Name = "labelMinWordOccurence";
            labelMinWordOccurence.Size = new Size(155, 15);
            labelMinWordOccurence.TabIndex = 19;
            labelMinWordOccurence.Text = "Minimum word occurrence:";
            // 
            // labelIgnoredWords
            // 
            labelIgnoredWords.AutoSize = true;
            labelIgnoredWords.Location = new Point(498, 393);
            labelIgnoredWords.Name = "labelIgnoredWords";
            labelIgnoredWords.Size = new Size(86, 15);
            labelIgnoredWords.TabIndex = 18;
            labelIgnoredWords.Text = "Ignored words:";
            // 
            // labelMinWordLength
            // 
            labelMinWordLength.AutoSize = true;
            labelMinWordLength.Location = new Point(498, 306);
            labelMinWordLength.Name = "labelMinWordLength";
            labelMinWordLength.Size = new Size(130, 15);
            labelMinWordLength.TabIndex = 17;
            labelMinWordLength.Text = "Minimum word length:";
            // 
            // listBoxCounter
            // 
            listBoxCounter.FormattingEnabled = true;
            listBoxCounter.ItemHeight = 15;
            listBoxCounter.Location = new Point(399, 3);
            listBoxCounter.Name = "listBoxCounter";
            listBoxCounter.Size = new Size(380, 274);
            listBoxCounter.TabIndex = 16;
            // 
            // textBox
            // 
            textBox.Location = new Point(3, 3);
            textBox.Multiline = true;
            textBox.Name = "textBox";
            textBox.ReadOnly = true;
            textBox.ScrollBars = ScrollBars.Vertical;
            textBox.Size = new Size(380, 274);
            textBox.TabIndex = 15;
            // 
            // DokuStatControl
            // 
            AutoScaleDimensions = new SizeF(7F, 15F);
            AutoScaleMode = AutoScaleMode.Font;
            Controls.Add(textBoxIgnoredWords);
            Controls.Add(spinBoxMinOccurrence);
            Controls.Add(spinBoxMinLength);
            Controls.Add(labelCharacters);
            Controls.Add(labelNonWhitespaceCharacters);
            Controls.Add(labelSentences);
            Controls.Add(labelProperNouns);
            Controls.Add(labelColemanLieuIndex);
            Controls.Add(labelFleschReadingEase);
            Controls.Add(labelMinWordOccurence);
            Controls.Add(labelIgnoredWords);
            Controls.Add(labelMinWordLength);
            Controls.Add(listBoxCounter);
            Controls.Add(textBox);
            Name = "DokuStatControl";
            Size = new Size(787, 421);
            ((System.ComponentModel.ISupportInitialize)spinBoxMinOccurrence).EndInit();
            ((System.ComponentModel.ISupportInitialize)spinBoxMinLength).EndInit();
            ResumeLayout(false);
            PerformLayout();
        }

        #endregion

        private TextBox textBoxIgnoredWords;
        private NumericUpDown spinBoxMinOccurrence;
        private NumericUpDown spinBoxMinLength;
        private Label labelCharacters;
        private Label labelNonWhitespaceCharacters;
        private Label labelSentences;
        private Label labelProperNouns;
        private Label labelColemanLieuIndex;
        private Label labelFleschReadingEase;
        private Label labelMinWordOccurence;
        private Label labelIgnoredWords;
        private Label labelMinWordLength;
        private ListBox listBoxCounter;
        private TextBox textBox;
    }
}
