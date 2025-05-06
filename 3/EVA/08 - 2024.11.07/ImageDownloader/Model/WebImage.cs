using System.Net.Http;

namespace ImageDownloader.Model
{
    public class WebImage
    {
        private byte[] _data;
        private Uri _url;

        protected WebImage(Uri url, byte[] data)
        {
            _data = data;
            _url = url;
        }

        public byte[] Data => _data;
        public Uri Url => _url;

        public async static Task<WebImage> DownloadAsync(Uri url)
        {
            HttpClient client = new HttpClient();

            if (url == null) throw new ArgumentException("Empty url!");
            if (!url.IsAbsoluteUri) throw new ArgumentException("Absolute url!");

            byte[] data = await client.GetByteArrayAsync(url);

            return new WebImage(url, data);
        }

    }
}
