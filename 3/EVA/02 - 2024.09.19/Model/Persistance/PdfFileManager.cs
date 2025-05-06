using iText.Kernel.Pdf;
using iText.Kernel.Pdf.Canvas.Parser;
using System.Text;

namespace DokuStat.Model.Persistance
{
    internal class PdfFileManager : IFileManager
    {
        private readonly string _path;

        public PdfFileManager(string path)
        {
            _path = path;
        }

        public async Task<string> LoadAsync()
        {
            try
            {
                using PdfReader reader = new PdfReader(_path);
                using PdfDocument document = new PdfDocument(reader);

                return await Task.Run(() =>
                    {
                        StringBuilder sb = new StringBuilder();
                        for (int i = 1; i <= document.GetNumberOfPages(); i++)
                        {
                            sb.Append(PdfTextExtractor.GetTextFromPage(document.GetPage(i)));
                        }

                        return sb.ToString();
                    });
            }
            catch (Exception ex)
            {
                throw new FileManagerException(ex.Message, ex);
            }
        }
    }
}
