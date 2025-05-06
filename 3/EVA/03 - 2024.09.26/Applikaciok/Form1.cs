namespace Applikaciok
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
            //quitButton.Click += ButtonClicked;        itt is lehet feliratkozni eseményekre
        }

        private void ButtonClicked(object sender, EventArgs e)
        {
            Application.Exit();
        }
    }
}
