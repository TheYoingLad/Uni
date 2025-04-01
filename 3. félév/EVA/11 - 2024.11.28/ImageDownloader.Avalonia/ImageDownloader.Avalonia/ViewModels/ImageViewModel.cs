using Avalonia.Media.Imaging;
using System;

namespace ImageDownloader.Avalonia.ViewModels
{
    public class ImageViewModel : ViewModelBase
    {
        public Bitmap Image { get; private set; }

        public DelegateCommand SaveImageCommand { get; private set; }
        public DelegateCommand CloseCommand { get; private set; }

        public event EventHandler<Bitmap>? SaveImage;
        public event EventHandler? CloseImage;

        public ImageViewModel(Bitmap image)
        {
            Image = image;

            SaveImageCommand = new DelegateCommand(_ =>
            {
                SaveImage?.Invoke(this, Image);
            });

            CloseCommand = new DelegateCommand(_ =>
            {
                CloseImage?.Invoke(this, EventArgs.Empty);
            });
        }
    }
}
