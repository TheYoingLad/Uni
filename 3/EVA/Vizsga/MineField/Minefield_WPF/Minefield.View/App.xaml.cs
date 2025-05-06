using Microsoft.Win32;
using Minefield.Model;
using Minefield.Persistence;
using Minefield.WPF.ViewModel;
using Minefield.View;
using System.ComponentModel;
using System.Configuration;
using System.Data;
using System.Windows;
using System.Windows.Controls.Primitives;

namespace Minefield.WPF
{
    /// <summary>
    /// Interaction logic for App.xaml
    /// </summary>
    public partial class App : Application, IDisposable
    {
        #region Fields

        private MinefieldGameModel _model = null!;      // modell
        private MinefieldViewModel _viewModel = null!;  // nézetmodell
        private MainWindow _view = null!;               // nézet

        #endregion

        #region Constructor

        /// <summary>
        /// Alkalmazást példányosító konstruktor
        /// </summary>
        public App()
        {
            Startup += new StartupEventHandler(AppStartup);
        }

        #endregion

        #region App event handlers

        /// <summary>
        /// Alkalmazás indulása eseménykezelő
        /// </summary>
        private void AppStartup(Object? sender, StartupEventArgs se)
        {
            IMinefieldFileAccess fileAccess = new MinefieldFileAccess();
            _model = new MinefieldGameModel(fileAccess);
            _model.GameOver += new EventHandler<MinefieldEventArgs>(Model_GameOver);

            _viewModel = new MinefieldViewModel(_model);
            _viewModel.NewGame += new EventHandler(ViewModel_NewGame);
            _viewModel.SaveGame += new EventHandler(ViewModel_SaveGame);
            _viewModel.LoadGame += new EventHandler(ViewModel_LoadGame);
            _viewModel.MovementKeyPressed += new EventHandler<MinefieldVMMovementEventArgs>(ViewModel_MovementKeyPressed);
            _viewModel.KeyPressedSpace += new EventHandler(ViewModel_KeyPressedSpace);

            _view = new MainWindow();
            _view.DataContext = _viewModel;
            _view.Closing += new System.ComponentModel.CancelEventHandler(View_Closing);
            _view.Show();
        }

        #endregion

        #region Model event handlers

        /// <summary>
        /// Játék vége eseménykezelő
        /// </summary>
        private void Model_GameOver(object? sender, MinefieldEventArgs e)
        {
            MessageBox.Show($"Time survived: {(int)e.Time.TotalMinutes:0}:{e.Time.Seconds:00}.{e.Time.Milliseconds / 100:0}", "Game Over",
                            MessageBoxButton.OK, MessageBoxImage.Information);
        }

        #endregion

        #region ViewModel event handlers

        /// <summary>
        /// Szóköz billentyű megnyomása eseménykezelő
        /// </summary>
        private void ViewModel_KeyPressedSpace(object? sender, EventArgs e)
        {
            _model.TogglePaused();
        }

        /// <summary>
        /// Mozgást irányító billentyű megnyomása eseménykezelő
        /// </summary>
        private void ViewModel_MovementKeyPressed(object? sender, MinefieldVMMovementEventArgs e) => _model.MovePlayer(e.Dir);

        /// <summary>
        /// Új játék eseménykezelő
        /// </summary>
        private void ViewModel_NewGame(object? sender, EventArgs e)
        {
            _model.StartNewGame();
        }

        /// <summary>
        /// Játék mentése eseménykezelő
        /// </summary>
        private void ViewModel_SaveGame(object? sender, EventArgs e)
        {
            try
            {
                SaveFileDialog saveFileDialog = new SaveFileDialog();
                saveFileDialog.Title = "Save game";
                saveFileDialog.Filter = "Minefield Save File|*.mfs";
                if (saveFileDialog.ShowDialog() == true)
                {
                    _model.SaveGame(saveFileDialog.FileName);   
                }
            }
            catch
            {
                MessageBox.Show("Incorrect path or format.", "Error", MessageBoxButton.OK, MessageBoxImage.Error);
            }
        }

        /// <summary>
        /// Játék betöltése eseménykezelő
        /// </summary>
        private void ViewModel_LoadGame(object? sender, EventArgs e)
        {
            try
            {
                OpenFileDialog openFileDialog = new OpenFileDialog();
                openFileDialog.Title = "Load game";
                openFileDialog.Filter = "Minefield Save File|*.mfs";
                if (openFileDialog.ShowDialog() == true)
                {
                    _model.LoadGame(openFileDialog.FileName);
                }
            }
            catch (MinefieldFileAccessException)
            {
                MessageBox.Show("Incorrect path or access denied.", "Error", MessageBoxButton.OK, MessageBoxImage.Error);
            }
        }

        #endregion

        #region View event handlers

        /// <summary>
        /// Nézet záródása eseménykezelő
        /// </summary>
        private void View_Closing(object? sender, CancelEventArgs e)
        {
            if (!_model.IsGamePaused) _model.TogglePaused();
            if (MessageBox.Show("Are you sure you want to quit?", "Exit Application", MessageBoxButton.YesNo, MessageBoxImage.Question) == MessageBoxResult.No)
            {
                e.Cancel = true;
            }
        }

        #endregion

        #region Dispose

        /// <summary>
        /// Dis
        /// </summary>
        public void Dispose()
        {
            _model.Dispose();
        }

        #endregion
    }

}
