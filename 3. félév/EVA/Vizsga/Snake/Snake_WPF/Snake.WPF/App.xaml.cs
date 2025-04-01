using Microsoft.Win32;
using Snake.Model;
using Snake.Persistence;
using Snake.WPF.View;
using Snake.WPF.ViewModel;
using System.Windows;

namespace Snake.WPF
{
    /// <summary>
    /// Interaction logic for App.xaml
    /// </summary>
    public partial class App : Application, IDisposable
    {
        #region Fields

        // Control Classes
        private GameModel model = null!;
        private MainViewModel viewModel = null!;
        private MainWindow window = null!;

        #endregion

        #region Constructors
        
        public App()
        {
            Startup += new StartupEventHandler(AppStartup);
        }

        #endregion

        #region Application Event Methods

        // Creates window and shows it
        private void AppStartup(object? sender, StartupEventArgs e)
        {
            model = new GameModel();

            viewModel = new MainViewModel(model);
            viewModel.StartGame += ViewModelStartGame;
            viewModel.LoadMap += ViewModelLoadMap;
            viewModel.ExitGame += ViewModelExitGame;

            window = new MainWindow();
            window.DataContext = viewModel;
            window.WindowStartupLocation = WindowStartupLocation.CenterScreen;
            window.Show();

            model.ReloadModel(null, null, true);
        }

        #endregion

        #region ViewModel-to-App Event Methods

        // Runs when Play Button is pressed
        private void ViewModelStartGame(object? sender, EventArgs e)
        {
            if (model.IsWallsEmpty)
            {
                // If no model is present then ask whether to play on an empty map is wanted
                MessageBoxResult result = MessageBox.Show("No map has been loaded in. Are you sure to play on an empty map?",
                    "No map loaded", MessageBoxButton.YesNo, MessageBoxImage.Warning);

                if (result == MessageBoxResult.Yes)
                {
                    model.ReloadModel();

                    viewModel.MenuBoxVisibility = false;
                    model.StartGame(newGame: true);
                }
            }
            else
            {
                if (!model.Ingame) model.ResetGame();

                viewModel.MenuBoxVisibility = false;
                model.StartGame(newGame: !model.Ingame);
            }
        }

        // Runs when Load Map Button is pressed
        private void ViewModelLoadMap(object? sender, EventArgs e)
        {
            OpenFileDialog fileDialog = new OpenFileDialog();
            fileDialog.InitialDirectory = "";
            fileDialog.Filter = "save files (*.save)|*.save|All files (*.*)|*.*";
            fileDialog.RestoreDirectory = true;

            if (fileDialog.ShowDialog() == true)
            {
                try
                {
                    IDataAccess dataAccess = new SaveFileDataAccess(fileDialog.FileName);
                    model.ReloadModel(dataAccess);

                    viewModel.MapLoadingViewChange();
                }
                catch (Exception ex)
                {
                    MessageBox.Show($"Error loading map in: {ex.Message}", "ERROR",
                                    MessageBoxButton.OK, MessageBoxImage.Error);
                }
            }
        }

        // Runs when Exit Button is pressed
        private void ViewModelExitGame(object? sender, EventArgs e)
        {
            window?.Close();
        }

        #endregion

        void IDisposable.Dispose()
        {
            model?.Dispose();
        }
    }
}