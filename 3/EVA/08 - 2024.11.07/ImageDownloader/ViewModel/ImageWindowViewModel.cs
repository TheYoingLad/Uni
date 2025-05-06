using System.Windows.Media.Imaging;

namespace ImageDownloader.ViewModel
{
    public class ImageWindowViewModel : ViewModelBase
    {
        public ImageWindowViewModel(BitmapImage image)
        {
            Image = image;

            SaveImageCommand = new DelegateCommand(_ =>
            {
                SaveImage?.Invoke(this, Image);
            });
        }
        public BitmapImage Image { get; private set; }

        public DelegateCommand SaveImageCommand { get; private set; }

        public event EventHandler<BitmapImage>? SaveImage;
    }
}
