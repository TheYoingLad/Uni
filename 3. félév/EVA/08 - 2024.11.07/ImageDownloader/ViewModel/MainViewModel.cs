using ImageDownloader.Model;
using System.Collections.ObjectModel;
using System.IO;
using System.Windows.Media.Imaging;

namespace ImageDownloader.ViewModel
{
    internal class MainViewModel : ViewModelBase
    {
        private WebPage? _model;
        private bool _isDownloading;
        private float _progress;

        public MainViewModel()
        {
            Images = new ObservableCollection<BitmapImage>();

            DownloadCommand = new DelegateCommand(async param =>
            {
                if (!_isDownloading) await LoadAsync(new Uri(param?.ToString() ?? ""));
                else CancelLoad();
            });

            ImageSelectedCommand = new DelegateCommand(param =>
            {
                if (param is BitmapImage bitmap) ImageSelected?.Invoke(this, bitmap);
            });
        }

        public bool IsDownloading
        {
            get => _isDownloading;
            private set
            {
                _isDownloading = value;
                OnPropertyChanged();
                OnPropertyChanged(nameof(DownloadButtonLabel));
            }
        }
        public int Progress
        {
            get => (int)_progress;
            private set
            {
                _progress = value;
                OnPropertyChanged();
            }
        }
        public ObservableCollection<BitmapImage> Images { get; set; }
        public string DownloadButtonLabel
        {
            get => _isDownloading ? "Betöltés megszakítása" : "Képek betöltése";
        }

        public DelegateCommand DownloadCommand { get; set; }
        public DelegateCommand ImageSelectedCommand { get; set; }

        public event EventHandler<BitmapImage>? ImageSelected;

        private void OnImageLoaded(object? sender, WebImage e)
        {
            var bitmap = new BitmapImage();
            bitmap.BeginInit();
            bitmap.StreamSource = new MemoryStream(e.Data);
            bitmap.EndInit();

            Images.Add(bitmap);
        }
        private void OnLoadProgress(object? sender, int e)
        {
            Progress = e;
        }
        public async Task LoadAsync(Uri url)
        {
            IsDownloading = true;
            Images.Clear();

            _model = new WebPage(url);
            _model.ImageLoaded += OnImageLoaded;
            _model.LoadProgress += OnLoadProgress;
            await _model.LoadImageAsync();

            IsDownloading = false;
        }
        private void CancelLoad()
        {
            if (IsDownloading) _model?.CancelLoad();
        }
    }
}
