using HtmlAgilityPack;
using System.Net.Http;
using System.Security.Policy;

namespace ImageDownloader.Model
{
    public class WebPage
    {
        private Uri _baseUrl;
        private ICollection<WebImage> _images;
        private CancellationTokenSource _cancelSource;
        private CancellationToken _cancelToken;

        public WebPage(Uri baseUrl)
        {
            if (baseUrl == null) throw new ArgumentException("Empty url!");
            if (!baseUrl.IsAbsoluteUri) throw new ArgumentException("Absolute url!");

            _baseUrl = baseUrl;
            _images = new List<WebImage>();
            _cancelSource = new CancellationTokenSource();
            _cancelToken = _cancelSource.Token;
        }

        public Uri BaseUrl => _baseUrl;
        public int ImageCount => _images.Count;
        public ICollection<WebImage> Images => _images;

        public event EventHandler<WebImage>? ImageLoaded;
        public event EventHandler<int>? LoadProgress;

        public async Task LoadImageAsync()
        {
            HttpClient client = new HttpClient();
            var response = await client.GetAsync(BaseUrl, _cancelToken);
            var content = await response.Content.ReadAsStringAsync();

            HtmlDocument doc = new HtmlDocument();
            doc.LoadHtml(content);

            var nodes = doc.DocumentNode.SelectNodes("//img");
            if (nodes == null) throw new ArgumentException("Parse unsuccessful");

            _images.Clear();
            int imageCount = 0;
            foreach (var node in nodes)
            {
                if (_cancelToken.IsCancellationRequested) break;
                if (!node.Attributes.Contains("src")) continue;

                Uri imageUrl = new Uri(node.Attributes["src"].Value, UriKind.RelativeOrAbsolute);

                imageCount++;
                LoadProgress?.Invoke(this, (imageCount * 100) / nodes.Count);
                
                if (!imageUrl.IsAbsoluteUri) imageUrl = new Uri(BaseUrl, imageUrl);

                try
                {
                    WebImage image = await WebImage.DownloadAsync(imageUrl);
                    _images.Add(image);

                    ImageLoaded?.Invoke(this, image);
                }
                catch { }
            }
        }
        public void CancelLoad()
        {
            _cancelSource.Cancel();
        }
    }
}
