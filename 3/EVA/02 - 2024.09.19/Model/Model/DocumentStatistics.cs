using DokuStat.Model.Persistance;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DokuStat.Model.Model
{
    public class DocumentStatistics : IDocumentStatistics
    {
        private readonly IFileManager _fileManager;

        #region Properties
        public string? FileContent { get; private set; }
        public Dictionary<string, int> DistinctWordCount { get; private set; }
        public int CharacterCount { get; private set; }
        public int NonWhiteSpaceCharacterCount { get; private set; }
        public int SentenceCount { get; private set; }
        public int ProperNounCount { get; private set; }
        public int ColemanLieuIndex { get; private set; }
        public int FleschReadingEase { get; private set; }
        #endregion

        #region Events
        public event EventHandler? FileContentReady;
        public event EventHandler? TextStatisticsReady;
        #endregion

        #region Constructor
        public DocumentStatistics(IFileManager fileManager)
        {
            _fileManager = fileManager;
            DistinctWordCount = new Dictionary<string, int>();
        }
        #endregion

        #region Public Methods
        public async Task LoadAsync()
        {
            FileContent = await _fileManager.LoadAsync();
            OnFileContentReady();

            await Task.Run(() =>
            {
                CharacterCount = FileContent.Length;
                NonWhiteSpaceCharacterCount = FileContent.Count(c => !char.IsWhiteSpace(c));

                ComputeSentenceCount();
                ComputeProperNounCount();
                ComputeDistinctWords();
                ComputeColemanLieuIndex();
                ComputeFleschReadingEase();
            });
            
            OnTextStatisticsReady();
        }
        public void ComputeDistinctWords()
        {
            if (FileContent == null) throw new ArgumentNullException("No file has been loaded!");
            string[] words = FileContent.Split().Where(s => s.Length > 0).ToArray();
            for (int i = 0; i < words.Length; i++)
            {
                while (words[i].Length > 0 && !char.IsLetter(words[i][0]))
                {
                    words[i] = words[i].Remove(0, 1);
                }
                while (words[i].Length > 0 && !char.IsLetter(words[i][^1]))
                {
                    words[i] = words[i].Remove(words[i].Length - 1, 1);
                }
                /*
                // szintén jó megoldás:
                string.Concat(words[i]
                      .SkipWhile(c => !char.IsLetter(c))
                      .Reverse()
                      .SkipWhile(c => !char.IsLetter(c))
                      .Reverse());
                */
                if (string.IsNullOrEmpty(words[i])) continue;
                words[i] = words[i].ToLower();
                if (DistinctWordCount.ContainsKey(words[i])) DistinctWordCount[words[i]]++;
                else DistinctWordCount.Add(words[i], 1);
            }
        }
        #endregion

        #region Calculator Methods
        private void ComputeSentenceCount()
        {
            if (FileContent == null) throw new ArgumentNullException("No file has been loaded!");
            SentenceCount = 0;
            string[] words = FileContent.Split().Where(s => s.Length > 0).ToArray();
            List<char> symbols = new List<char> { '!', '.', '?' };
            for (int i = 0; i < words.Length; i++) if (symbols.Contains(words[i][^1])) SentenceCount++;
        }
        private void ComputeProperNounCount()
        {
            if (FileContent == null) throw new ArgumentNullException("No file has been loaded!");
            ProperNounCount = 0;
            string[] words = FileContent.Split().Where(s => s.Length > 0).ToArray();
            List<char> symbols = new List<char> { '!', '.', '?' };
            for (int i = 1; i < words.Length; i++) if (char.IsUpper(words[i][0]) && !symbols.Contains(words[i - 1][^1])) ProperNounCount++;
        }
        private void ComputeColemanLieuIndex()
        {
            if (FileContent == null) throw new ArgumentNullException("No file has been loaded!");
            int totalWordCount = DistinctWordCount.Sum(w => w.Value);
            if (totalWordCount == 0) throw new DivideByZeroException("The file contains no words!");
            ColemanLieuIndex = (int)(0.0588 * (100 * NonWhiteSpaceCharacterCount / totalWordCount) - 0.296 * (100 * SentenceCount / totalWordCount) - 15.8);
        }
        private int CountSyllables(string w)
        {
            if (w.Length == 0) return 0;
            List<char> vowels = new List<char> { 'a', 'e', 'i', 'o', 'u', 'y' };
            if (w.Length == 1) return vowels.Contains(w[0]) ? 1 : 0;
            int syllables = 0;
            if (vowels.Contains(w[0])) syllables++;
            for (int i = 1; i < w.Length; i++) if (vowels.Contains(w[i]) && !vowels.Contains(w[i - 1])) syllables++;
            return syllables;
        }
        private void ComputeFleschReadingEase()
        {
            if (FileContent == null) throw new ArgumentNullException("No file has been loaded!");
            int totalWordCount = DistinctWordCount.Sum(w => w.Value);
            if (totalWordCount == 0) throw new DivideByZeroException("The file contains no words!");
            int totalSyllableCount = DistinctWordCount.Sum(w => w.Value * CountSyllables(w.Key));
            if (totalSyllableCount == 0) throw new DivideByZeroException("The file contains no syllables!");
            FleschReadingEase = (int)(206.835 - 1.015 * (totalWordCount / SentenceCount) - 84.6 * (totalSyllableCount / totalWordCount));
        }
        #endregion

        #region Event Methods
        private void OnFileContentReady()
        {
            FileContentReady?.Invoke(this, EventArgs.Empty);
        }
        private void OnTextStatisticsReady()
        {
            TextStatisticsReady?.Invoke(this, EventArgs.Empty);
        }
        #endregion
    }
}
