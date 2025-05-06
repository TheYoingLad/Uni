using Bag; 

namespace Test_Bag
{
    // Ez az attrib�tom kell a teszt oszt�lyok el�
    [TestClass]
    public class UnitTest_Bag
    {
        // Ez az attrib�tom kell a teszt met�dusok el�
        [TestMethod]
        public void TestPelda()
        {
            // Igazs�g vizsg�lat
            Assert.IsTrue(5 > 0, "Az �tnek t�bbnek kell lennie a null�n�l");
            // Hamiss�g vizsg�lat
            Assert.IsFalse(5 > 7, "Az �t nem lehet t�bb a h�tn�l");
            // Egyenl�s�g vizsg�lat
            Assert.AreEqual("kacsa", "kacsa", "A k�t kacs�nak azonosnak k�ne lennie");
            // Hiba vizsg�lat
            Assert.ThrowsException<ArgumentException>(() => throw new ArgumentException(), "ArgumentExceptionnek k�ne fell�pnie");
        }

        [TestMethod]
        // Ez az attrib�tum is hib�t vizsg�l
        [ExpectedException(typeof(ArgumentException))]
        public void TestHiba()
        {
            throw new ArgumentException();
        }

        [TestMethod]
        // Ezek az attrib�tumok t�bbsz�r is megh�vj�t a teszt met�dust
        [DataRow(0)]
        [DataRow(1)]
        [DataRow(-1)]
        public void TestTobbAdat(int num)
        {
            Assert.IsTrue(SegedMetodus() > num, $"Az �tnek t�bbnek kell lennie a {num} sz�mn�l");
        }

        // Term�szetesen lehetnek seg�dmet�dusaink attrib�tumok n�lk�l
        private int SegedMetodus()
        {
            return 5;
        }

        // Lehetnek v�ltoz�ink is
        private string ertek;
        // Ez lefut minden teszt el�tt
        /*[TestInitialize]
        public void Inicializalas()
        {
            ertek = "kincs";
        }*/

        // Ez pedig minden teszt ut�n
        [TestCleanup]
        public void Tisztitas()
        {
            ertek = "semmi";
        }


        private Bag.Bag b;


        [TestInitialize]
        public void Initialisation()
        {
            b = new Bag.Bag();
        }

        [TestMethod]
        public void TestKonstruktor()
        {
            Assert.IsTrue(b.Empty());
        }

        [TestMethod]
        public void TestInsertRemove()
        {
            b.Insert("jaj");
            Assert.IsFalse(b.Empty());
            b.Remove("jaj");
            Assert.IsTrue(b.Empty());

            b.Insert("jaj");
            b.Insert("ja");
            b.Insert("ja");
            Assert.IsFalse(b.Empty());
        }
    }
}