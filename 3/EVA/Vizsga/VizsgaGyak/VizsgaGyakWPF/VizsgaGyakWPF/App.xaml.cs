using Microsoft.Win32;
using System.Windows;
using VizsgaGyakWPF.Model;
using VizsgaGyakWPF.View;
using VizsgaGyakWPF.ViewModel;

namespace VizsgaGyakWPF
{
    /// <summary>
    /// Interaction logic for App.xaml
    /// </summary>
    public partial class App : Application
    {
        #region Fields
        private AttackModel _model = null!;
        private AttackViewModel _viewModel = null!;
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
            _model = new AttackModel(6);

            _viewModel = new AttackViewModel(_model);

            _view = new MainWindow();
            _view.DataContext = _viewModel;
            _view.Show();
        }
        #endregion

        //#region Model event handlers
        //private void Model_GameOver(object? sender, bool e)
        //{
        //    switch (e)
        //    {
        //        case true:
        //            MessageBox.Show("Player 1 won!", "Game Over", MessageBoxButton.OK, MessageBoxImage.Information);
        //            break;
        //        case false:
        //            MessageBox.Show("Player 2 won!", "Game Over", MessageBoxButton.OK, MessageBoxImage.Information);
        //            break;
        //        default:
        //            MessageBox.Show("It's a Draw!", "Game Over", MessageBoxButton.OK, MessageBoxImage.Information);
        //            break;
        //    }

        //    if (MessageBox.Show("Play Again?", "New Game", MessageBoxButton.YesNo, MessageBoxImage.Question) == MessageBoxResult.Yes)
        //    {
        //        _model.NewGame();
        //    }
        //    else _view.Close();
        //}
        //#endregion

        //#region ViewModel event handlers
        //private void ViewModel_NewGame(object? sender, int n)
        //{
        //    if (MessageBox.Show($"Start new game?\n\nNew grid size: {n}", "New Game", MessageBoxButton.YesNo, MessageBoxImage.Question) == MessageBoxResult.Yes)
        //    {
        //        _model.Size = n;
        //        _model.NewGame();
        //    }
        //}
        //#endregion
    }

}
