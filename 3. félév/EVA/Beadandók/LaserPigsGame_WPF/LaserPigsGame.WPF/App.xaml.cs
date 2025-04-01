using LaserPigs.Model;
using LaserPigs.Persistence;
using LaserPigsGame.View;
using LaserPigsGame.ViewModel;
using Microsoft.Win32;
using System.Windows;

namespace LaserPigsGame
{
    /// <summary>
    /// Interaction logic for App.xaml
    /// </summary>
    public partial class App : Application
    {
        #region Fields
        private LaserPigsModel _model = null!;
        private LaserPigsViewModel _viewModel = null!;
        private MainWindow _view = null!;
        #endregion

        #region Constructor
        /// <summary>
        /// Constructs the application
        /// </summary>
        public App()
        {
            Startup += OnStartup;
        }
        #endregion

        #region App event handlers
        private void OnStartup(object? sender, StartupEventArgs e)
        {
            _model = new LaserPigsModel(new FileManager());
            _model.GameOver += Model_GameOver;
            _model.NewGame();

            _viewModel = new LaserPigsViewModel(_model);
            _viewModel.NewGame += ViewModel_NewGame;
            _viewModel.LoadGame += ViewModel_LoadGame;
            _viewModel.SaveGame += ViewModel_SaveGame;
            _viewModel.ExitGame += ViewModel_ExitGame;

            _view = new MainWindow();
            _view.DataContext = _viewModel;
            _view.Show();
        }
        #endregion

        #region Model event handlers
        private void Model_GameOver(object? sender, GameOverEventArgs e)
        {
            switch (e.GetWinner)
            {
                case true:
                    MessageBox.Show("Player 1 won!", "Game Over", MessageBoxButton.OK, MessageBoxImage.Information);
                    break;
                case false:
                    MessageBox.Show("Player 2 won!", "Game Over", MessageBoxButton.OK, MessageBoxImage.Information);
                    break;
                default:
                    MessageBox.Show("It's a Draw!", "Game Over", MessageBoxButton.OK, MessageBoxImage.Information);
                    break;
            }

            if (MessageBox.Show("Play Again?", "New Game", MessageBoxButton.YesNo, MessageBoxImage.Question) == MessageBoxResult.Yes)
            {
                _model.NewGame();
            }
            else _view.Close();
        }
        #endregion

        #region ViewModel event handlers
        private void ViewModel_NewGame(object? sender, int n)
        {
            if (MessageBox.Show($"Start new game?\n\nNew grid size: {n}", "New Game", MessageBoxButton.YesNo, MessageBoxImage.Question) == MessageBoxResult.Yes)
            {
                _model.Size = n;
                _model.NewGame();
            }
        }
        private async void ViewModel_LoadGame(object? sender, EventArgs e)
        {
            try
            {
                OpenFileDialog openFileDialog = new OpenFileDialog
                {
                    Filter = "LaserPig files (*.lspf)|*.lspf",
                    Title = "Load LaserPig file"
                };
                if (openFileDialog.ShowDialog() == true) await _model.LoadGameAsync(openFileDialog.FileName);
            }
            catch (Exception ex)
            {
                MessageBox.Show("An error occured:\n" + ex.Message, "Error", MessageBoxButton.OK, MessageBoxImage.Error);
                MessageBox.Show("File could not be loaded, restarting game", "Restart", MessageBoxButton.OK, MessageBoxImage.Information);

                _model.NewGame();
            }
        }
        private async void ViewModel_SaveGame(object? sender, EventArgs e)
        {
            try
            {
                SaveFileDialog saveFileDialog = new SaveFileDialog
                {
                    Filter = "LaserPig files (*.lspf)|*.lspf",
                    Title = "Save LaserPig file"
                };

                if (saveFileDialog.ShowDialog() == true) await _model.SaveGameAsync(saveFileDialog.FileName);
            }
            catch (Exception ex)
            {
                MessageBox.Show("An error occured:\n" + ex.Message, "Error", MessageBoxButton.OK, MessageBoxImage.Error);
            }
        }
        private void ViewModel_ExitGame(object? sender, EventArgs e)
        {
            if (MessageBox.Show("Are you sure you want to quit?", "Exit", MessageBoxButton.YesNo, MessageBoxImage.Question) == MessageBoxResult.Yes) _view.Close();
        }
        #endregion
    }
}
