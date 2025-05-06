using System;
using System.Windows.Media.Imaging;

namespace ELTE.ImageDownloader.ViewModel
{
    public class ImageWindowViewModel : ViewModelBase
    {
        public BitmapImage Image { get; private set; }

        public DelegateCommand SaveImageCommand { get; private set; }

        public event EventHandler<BitmapImage>? SaveImage;

        public ImageWindowViewModel(BitmapImage image)
        {
            Image = image;

            SaveImageCommand = new DelegateCommand(_ =>
            {
                SaveImage?.Invoke(this, Image);
            });
        }
    }
}
