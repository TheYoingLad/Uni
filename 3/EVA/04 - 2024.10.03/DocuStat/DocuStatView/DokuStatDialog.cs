using DokuStat.Model.Model;
using DokuStat.Model.Persistance;

namespace DocuStatView
{
    public partial class DokuStatDialog : Form
    {
        public DokuStatDialog()
        {
            InitializeComponent();

            openFileDialogMenuItem.Click += OpenDialog;
            countWordsMenuItem.Click += CountWords;
        }

        private async void OpenDialog(object? sender, EventArgs e)
        {
            using (OpenFileDialog openFileDialog = new OpenFileDialog())
            {
                openFileDialog.InitialDirectory = ".\\";
                openFileDialog.Filter = "txt files (*.txt)|*.txt|PDF files (*.pdf)|*.pdf|All files (*.*)|*.*";
                openFileDialog.RestoreDirectory = true;
                openFileDialog.Multiselect = true;

                if (openFileDialog.ShowDialog() == DialogResult.OK)
                {
                    string[] fileNames = openFileDialog.FileNames;
                    List<Task> tasks = new List<Task>();

                    foreach (string fileName in fileNames)
                    {
                        tasks.Add(AddTabPageAsync(fileName));
                    }

                    await Task.WhenAll(tasks);
                }
            }
        }

        private async Task AddTabPageAsync (string fileName)
        {
            IFileManager? fileManager = FileManagerFactory.CreateForPath(fileName);

            if (fileManager == null)
            {
                MessageBox.Show("File reading was unsuccessful!\nUnsupported file format.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                return;
            }

            try
            {
                DokuStatControl control = new DokuStatControl();
                Task loadTask =  control.LoadFileAsync(fileManager);

                TabPage page = new TabPage("Loading...");
                page.Controls.Add(control);
                tabControl.TabPages.Add(page);

                openFileDialogMenuItem.Enabled = false;
                countWordsMenuItem.Enabled = false;
                await loadTask;
                openFileDialogMenuItem.Enabled = true;
                countWordsMenuItem.Enabled = true;
                page.Text = Path.GetFileName(fileName);
            }
            catch (FileManagerException ex)
            {
                MessageBox.Show("File reading was unsuccessful!\n" + ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                return;
            }
        }

        private void CountWords(object? sender, EventArgs e)
        {
            if (tabControl.SelectedTab != null) (tabControl.SelectedTab.Controls[0] as DokuStatControl)!.CalculateStatistics();
        }
    }
}
