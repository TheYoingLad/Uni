using DokuStat.Model.Model;
using DokuStat.Model.Persistance;
using Moq;

namespace DokuStat.Test
{
    [TestClass]
    public class DokuStatTest
    {
        private Mock<IFileManager> _fileManagerMock;
        private DocumentStatistics? _documentStatistics;

        public DokuStatTest()
        {

        }

        [TestInitialize]
        public void Initialize()
        {
            _fileManagerMock = new Mock<IFileManager>();
            _documentStatistics = new DocumentStatistics(_fileManagerMock.Object);
        }

        [TestCleanup]
        public void Cleanup()
        {
            _documentStatistics = null;
        }

        [TestMethod]
        public void TestEmptyWord()
        {
            MockAndLoadString("");
            _documentStatistics!.Load();

            Assert.AreEqual("", _documentStatistics.FileContent);
        }
        [DataTestMethod]
        [DataRow("")]
        [DataRow("teszt")]
        public void TestText(string text)
        {
            MockAndLoadString(text);
            _documentStatistics!.Load();

            Assert.AreEqual(text, _documentStatistics.FileContent);
        }

        [TestMethod]
        [ExpectedException(typeof(ArgumentException))]
        public void TestArgumentExceptionA()
        {
            MockAndThrowException();
            _documentStatistics!.Load();
        }
        [TestMethod]
        public void TestArgumentExceptionB()
        {
            MockAndThrowException();

            Assert.ThrowsException<ArgumentException>(() => _documentStatistics!.Load());
        }

        [TestMethod]
        public void TestEvents()
        {
            MockAndLoadString("");
            
            bool fileContentReadyEventFired = false;

            _documentStatistics!.FileContentReady += (sender, args) =>
            {
                fileContentReadyEventFired = true; //inline event handler
            };

            _documentStatistics!.Load();

            Assert.AreEqual(true, fileContentReadyEventFired);
        }


        [TestMethod]
        public void TestDistinctWordsCountEmpty()
        {
            MockAndLoadString("");
            _documentStatistics!.Load();

            Assert.AreEqual(0, _documentStatistics.DistinctWordCount.Count);
        }
        [TestMethod]
        public void TestDistinctWordsCountOnlyNonLetter()
        {
            MockAndLoadString(" 12 !! \n \t ");
            _documentStatistics!.Load();

            Assert.AreEqual(0, _documentStatistics.DistinctWordCount.Count);
        }
        [TestMethod]
        public void TestDistinctWordsCountMultiple()
        {
            MockAndLoadString("alma alma alma");
            _documentStatistics!.Load();

            Assert.AreEqual(3, _documentStatistics.DistinctWordCount["alma"]);
        }
        [TestMethod]
        public void TestDistinctWordsCountMultipleWithNonLetters()
        {
            MockAndLoadString("alma! alma4 ,alma");
            _documentStatistics!.Load();

            Assert.AreEqual(3, _documentStatistics.DistinctWordCount["alma"]);
        }
        [TestMethod]
        public void TestDistinctWordsCountMultipleCapital()
        {
            MockAndLoadString("alma Alma ALMA");
            _documentStatistics!.Load();

            Assert.AreEqual(3, _documentStatistics.DistinctWordCount["alma"]);
        }
        public void TestDistinctWordsCountMultipleDifferent()
        {
            MockAndLoadString("alma bela alma jani bela");
            _documentStatistics!.Load();

            Assert.AreEqual(2, _documentStatistics.DistinctWordCount["alma"]);
            Assert.AreEqual(2, _documentStatistics.DistinctWordCount["bela"]);
            Assert.AreEqual(1, _documentStatistics.DistinctWordCount["jani"]);
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