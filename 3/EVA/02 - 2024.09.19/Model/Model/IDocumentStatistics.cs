using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DokuStat.Model.Model
{
    public interface IDocumentStatistics
    {
        public string? FileContent { get; }
        public Dictionary<string, int> DistinctWordCount { get; }
        public int CharacterCount { get; }
        public int NonWhiteSpaceCharacterCount { get; }
        public int SentenceCount { get; }
        public int ProperNounCount { get; }
        public int ColemanLieuIndex { get; }
        public int FleschReadingEase { get; }
        public event EventHandler? FileContentReady;
        public event EventHandler? TextStatisticsReady;
        public Task LoadAsync();
        public void ComputeDistinctWords();
    }
}
