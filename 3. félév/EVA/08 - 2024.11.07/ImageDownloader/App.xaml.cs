using ImageDownloader.View;
using ImageDownloader.ViewModel;
using Microsoft.Win32;
using System.IO;
using System.Windows;
using System.Windows.Media.Imaging;

namespace ImageDownloader
{
    /// <summary>
    /// Interaction logic for App.xaml
    /// </summary>
    public partial class App : Application
    {
        private MainViewModel? _mainViewModel;
        private MainWindow? _mainWindow;
        public App()
        {
            Startup += OnStartup;
        }

        private void OnStartup(object sender, StartupEventArgs e)
        {
            _mainViewModel = new MainViewModel();
            _mainViewModel.ImageSelected += ImageSelected;

            _mainWindow = new MainWindow();
            _mainWindow.DataContext = _mainViewModel;
            _mainWindow.Show();
        }
        private void ImageSelected(object? sender, BitmapImage e)
        {
            ImageWindowViewModel viewModel = new ImageWindowViewModel(e);
            viewModel.SaveImage += SaveImage;

            ImageWindow window = new ImageWindow();
            window.Owner = _mainWindow;
            window.DataContext = viewModel;
            window.Show();
        }
        private void SaveImage(object? sender, BitmapImage e)
        {
            SaveFileDialog saveFileDialog = new SaveFileDialog();
            saveFileDialog.Filter = "PNG files (*.png)|*.png";
            saveFileDialog.InitialDirectory = Environment.GetFolderPath(Environment.SpecialFolder.MyPictures);
            saveFileDialog.RestoreDirectory = true;

            if(saveFileDialog.ShowDialog() == true)
            {
                using (FileStream fileStream = new FileStream(saveFileDialog.FileName, FileMode.Create))
                {
                    BitmapEncoder encoder = new PngBitmapEncoder();
                    encoder.Frames.Add(BitmapFrame.Create(e));
                    encoder.Save(fileStream);
                }
            }
        }
    }
}
