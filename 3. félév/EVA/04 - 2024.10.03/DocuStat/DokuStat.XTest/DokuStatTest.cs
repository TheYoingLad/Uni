using DokuStat.Model.Model;
using DokuStat.Model.Persistance;
using Moq;
using Xunit.Sdk;

namespace DokuStat.XTest
{
    public class DokuStatTest : IDisposable
    {
        private Mock<IFileManager> _fileManagerMock;
        private DocumentStatistics? _documentStatistics;
        //Initialize
        public DokuStatTest()
        {
            _fileManagerMock = new Mock<IFileManager>();
            _documentStatistics = new DocumentStatistics(_fileManagerMock.Object);
        }
        //Cleanup
        public void Dispose()
        {
            _documentStatistics = null;
        }

        [Fact]
        public void TestEmptyWord()
        {
            MockAndLoadString("");
            _documentStatistics!.Load();

            Assert.Equal("", _documentStatistics.FileContent);
        }
        [Theory]
        [InlineData("alma")]
        [InlineData("")]
        [InlineData("teszt")]
        public void TestText(string text)
        {
            MockAndLoadString(text);
            _documentStatistics!.Load();

            Assert.Equal(text, _documentStatistics.FileContent);
        }

        [Fact]
        public void TestArgumentExceptionB()
        {
            MockAndThrowException();

            Assert.Throws<ArgumentException>(() => _documentStatistics!.Load());
        }

        [Fact]
        public void TestEvents()
        {
            MockAndLoadString("");

            bool fileContentReadyEventFired = false;

            _documentStatistics!.FileContentReady += (sender, args) =>
            {
                fileContentReadyEventFired = true; //inline event handler
            };

            _documentStatistics!.Load();

            Assert.Equal(true, fileContentReadyEventFired);
        }


        [Fact]
        public void TestDistinctWordsCountEmpty()
        {
            MockAndLoadString("");
            _documentStatistics!.Load();

            Assert.Equal(0, _documentStatistics.DistinctWordCount.Count);
        }
        [Fact]
        public void TestDistinctWordsCountOnlyNonLetter()
        {
            MockAndLoadString(" 12 !! \n \t ");
            _documentStatistics!.Load();

            Assert.Equal(0, _documentStatistics.DistinctWordCount.Count);
        }
        [Fact]
        public void TestDistinctWordsCountMultiple()
        {
            MockAndLoadString("alma alma alma");
            _documentStatistics!.Load();

            Assert.Equal(3, _documentStatistics.DistinctWordCount["alma"]);
        }
        [Fact]
        public void TestDistinctWordsCountMultipleWithNonLetters()
        {
            MockAndLoadString("alma! alma4 ,alma");
            _documentStatistics!.Load();

            Assert.Equal(3, _documentStatistics.DistinctWordCount["alma"]);
        }
        [Fact]
        public void TestDistinctWordsCountMultipleCapital()
        {
            MockAndLoadString("alma Alma ALMA");
            _documentStatistics!.Load();

            Assert.Equal(3, _documentStatistics.DistinctWordCount["alma"]);
        }
        public void TestDistinctWordsCountMultipleDifferent()
        {
            MockAndLoadString("alma bela alma jani bela");
            _documentStatistics!.Load();

            Assert.Equal(2, _documentStatistics.DistinctWordCount["alma"]);
            Assert.Equal(2, _documentStatistics.DistinctWordCount["bela"]);
            Assert.Equal(1, _documentStatistics.DistinctWordCount["jani"]);
        }

        public void MockAndLoadString(string text)
        {
            _fileManagerMock.Setup(m => m.Load()).Returns(text);
        }
        public void MockAndThrowException()
        {
            _fileManagerMock.Setup(m => m.Load()).Throws(new ArgumentException());
        }
    }
}