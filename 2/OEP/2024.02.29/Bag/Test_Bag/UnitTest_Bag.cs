using Bag; 

namespace Test_Bag
{
    // Ez az attribútom kell a teszt osztályok elé
    [TestClass]
    public class UnitTest_Bag
    {
        // Ez az attribútom kell a teszt metódusok elé
        [TestMethod]
        public void TestPelda()
        {
            // Igazság vizsgálat
            Assert.IsTrue(5 > 0, "Az ötnek többnek kell lennie a nullánál");
            // Hamisság vizsgálat
            Assert.IsFalse(5 > 7, "Az öt nem lehet több a hétnél");
            // Egyenlõség vizsgálat
            Assert.AreEqual("kacsa", "kacsa", "A két kacsának azonosnak kéne lennie");
            // Hiba vizsgálat
            Assert.ThrowsException<ArgumentException>(() => throw new ArgumentException(), "ArgumentExceptionnek kéne fellépnie");
        }

        [TestMethod]
        // Ez az attribútum is hibát vizsgál
        [ExpectedException(typeof(ArgumentException))]
        public void TestHiba()
        {
            throw new ArgumentException();
        }

        [TestMethod]
        // Ezek az attribútumok többször is meghívját a teszt metódust
        [DataRow(0)]
        [DataRow(1)]
        [DataRow(-1)]
        public void TestTobbAdat(int num)
        {
            Assert.IsTrue(SegedMetodus() > num, $"Az ötnek többnek kell lennie a {num} számnál");
        }

        // Természetesen lehetnek segédmetódusaink attribútumok nélkül
        private int SegedMetodus()
        {
            return 5;
        }

        // Lehetnek változóink is
        private string ertek;
        // Ez lefut minden teszt elõtt
        /*[TestInitialize]
        public void Inicializalas()
        {
            ertek = "kincs";
        }*/

        // Ez pedig minden teszt után
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